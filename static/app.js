async function getFacts() {
  const r = await fetch("/api/facts");
  if (!r.ok) throw new Error("Impossible de charger les faits");
  return await r.json();
}

function selectedFacts() {
  const checks = document.querySelectorAll("input[type=checkbox][data-fact]");
  return [...checks].filter(c => c.checked).map(c => c.getAttribute("data-fact"));
}

function renderFacts(facts) {
  const root = document.getElementById("facts");
  root.innerHTML = "";
  for (const f of facts) {
    const id = `fact_${f.code}`;
    const row = document.createElement("label");
    row.className = "fact";
    row.innerHTML = `
      <input type="checkbox" id="${id}" data-fact="${f.code}">
      <span class="code">${f.code}</span>
      <span>${f.label}</span>
    `;
    root.appendChild(row);
  }
}

async function runDiagnostic() {
  const mode = document.getElementById("mode").value;
  const facts = selectedFacts();
  const goal = document.getElementById("goal").value.trim();

  const payload = { mode, facts };
  if (mode === "backward") payload.goal = goal;

  const r = await fetch("/api/diagnostic", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(payload),
  });

  const data = await r.json();
  document.getElementById("out").textContent = JSON.stringify(data, null, 2);
}

async function addFact() {
  const code = document.getElementById("newFactCode").value.trim();
  const label = document.getElementById("newFactLabel").value.trim();

  const r = await fetch("/api/facts", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ code, label }),
  });

  const data = await r.json();
  document.getElementById("out").textContent = JSON.stringify(data, null, 2);
}

async function addRule() {
  const id = document.getElementById("newRuleId").value.trim();
  const condsRaw = document.getElementById("newRuleConds").value.trim();
  const conclusion = document.getElementById("newRuleThen").value.trim();

  let conds;
  try {
    conds = JSON.parse(condsRaw);
  } catch {
    alert("Conditions invalides: mets un JSON valide, ex: [\"F1\",\"F10\"]");
    return;
  }

  const r = await fetch("/api/rules", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ id, conds, conclusion }),
  });

  const data = await r.json();
  document.getElementById("out").textContent = JSON.stringify(data, null, 2);
}

async function main() {
  const { facts } = await getFacts();
  renderFacts(facts);

  document.getElementById("run").addEventListener("click", runDiagnostic);
  document.getElementById("reloadFacts").addEventListener("click", async () => {
    const { facts } = await getFacts();
    renderFacts(facts);
  });
  document.getElementById("addFact").addEventListener("click", addFact);
  document.getElementById("addRule").addEventListener("click", addRule);
}

main().catch(e => {
  document.getElementById("out").textContent = String(e);
});
