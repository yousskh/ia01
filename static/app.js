async function loadFacts() {
  const r = await fetch("/api/facts");
  const j = await r.json();
  const root = document.getElementById("facts");
  root.innerHTML = "";
  j.facts.forEach(f => {
    root.innerHTML += `
      <label>
        <input type="checkbox" value="${f.code}">
        ${f.code} – ${f.label}
      </label><br>
    `;
  });
}

async function run() {
  const facts = [...document.querySelectorAll("input:checked")].map(i => i.value);
  const r = await fetch("/api/diagnostic", {
    method: "POST",
    headers: {"Content-Type":"application/json"},
    body: JSON.stringify({ mode:"forward", facts })
  });
  document.getElementById("out").textContent =
    JSON.stringify(await r.json(), null, 2);
}

loadFacts();
