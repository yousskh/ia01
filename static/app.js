let allFacts = [];
let allActions = [];
let currentMode = null;
let selectedFacts = [];
let selectedActions = [];
let currentStep = 0;
let categories = [];
let actionCategories = [];
let grouped = {};
let groupedActions = {};

async function loadFacts() {
  try {
    const response = await fetch("/api/facts");
    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }
    const data = await response.json();
    allFacts = data.facts || [];
    
    grouped = {};
    allFacts.forEach(fact => {
      const category = fact[2] || 'Autre';
      if (!grouped[category]) {
        grouped[category] = [];
      }
      grouped[category].push(fact);
    });
    
    categories = Object.keys(grouped).sort();
    renderAllCategories();
  } catch (error) {
    console.error("Erreur:", error);
    document.getElementById("currentCategoryFacts").innerHTML = 
      `<p>Erreur: ${error.message}</p>`;
  }
}

async function loadActions() {
  try {
    const response = await fetch("/api/actions");
    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }
    const data = await response.json();
    allActions = data.actions || [];
    
    groupedActions = {};
    allActions.forEach(action => {
      const category = action[1] || 'Autre';
      if (!groupedActions[category]) {
        groupedActions[category] = [];
      }
      groupedActions[category].push(action);
    });
    
    actionCategories = Object.keys(groupedActions).sort();
    renderAllActionCategories();
  } catch (error) {
    console.error("Erreur chargement actions:", error);
  }
}

function renderAllCategories() {
  const container = document.getElementById("currentCategoryFacts");
  container.innerHTML = "";
  
  categories.forEach((category, index) => {
    const categoryDiv = document.createElement("div");
    categoryDiv.className = "category-section";
    categoryDiv.id = `category-${index}`;
    categoryDiv.style.display = "none";
    
    const factList = document.createElement("div");
    factList.className = "fact-list";
    
    grouped[category].forEach(fact => {
      const factDiv = document.createElement("div");
      factDiv.className = "fact-item";
      
      const checkbox = document.createElement("input");
      checkbox.type = "checkbox";
      checkbox.value = fact[0];
      checkbox.id = `fact-${fact[0]}`;
      checkbox.className = "hidden-checkbox";
      if (selectedFacts.includes(fact[0])) {
        checkbox.checked = true;
        factDiv.classList.add("selected");
      }
      
      const label = document.createElement("label");
      label.className = "fact-label";
      label.setAttribute("for", `fact-${fact[0]}`);
      label.textContent = fact[1];
      
      factDiv.addEventListener('click', (e) => {
        e.preventDefault();
        checkbox.checked = !checkbox.checked;
        factDiv.classList.toggle("selected", checkbox.checked);
        updateSelectedFacts();
      });

      factDiv.appendChild(checkbox);
      factDiv.appendChild(label);
      factList.appendChild(factDiv);
    });
    
    categoryDiv.appendChild(factList);
    container.appendChild(categoryDiv);
  });
}

function renderAllActionCategories() {
  const container = document.getElementById("currentCategoryActions");
  container.innerHTML = "";
  
  actionCategories.forEach((category, index) => {
    const categoryDiv = document.createElement("div");
    categoryDiv.className = "category-section";
    categoryDiv.id = `action-category-${index}`;
    categoryDiv.style.display = "none";
    
    const actionList = document.createElement("div");
    actionList.className = "fact-list";
    
    groupedActions[category].forEach((action, actionIndex) => {
      const actionDiv = document.createElement("div");
      actionDiv.className = "fact-item";
      
      const actionId = `action-${index}-${actionIndex}`;
      const checkbox = document.createElement("input");
      checkbox.type = "checkbox";
      checkbox.value = action[0];
      checkbox.id = actionId;
      checkbox.className = "hidden-checkbox";
      if (selectedActions.includes(action[0])) {
        checkbox.checked = true;
        actionDiv.classList.add("selected");
      }
      
      const label = document.createElement("label");
      label.className = "fact-label";
      label.setAttribute("for", actionId);
      label.textContent = action[2];
      
      actionDiv.addEventListener('click', (e) => {
        e.preventDefault();
        checkbox.checked = !checkbox.checked;
        actionDiv.classList.toggle("selected", checkbox.checked);
        updateSelectedActions();
      });

      actionDiv.appendChild(checkbox);
      actionDiv.appendChild(label);
      actionList.appendChild(actionDiv);
    });
    
    categoryDiv.appendChild(actionList);
    container.appendChild(categoryDiv);
  });
}

function showActionCategory(step) {
  actionCategories.forEach((_, index) => {
    const categoryDiv = document.getElementById(`action-category-${index}`);
    if (categoryDiv) {
      categoryDiv.style.display = index === step ? "block" : "none";
    }
  });
  const title = document.getElementById("stepTitle");
  if(currentMode === "backward" && actionCategories[step]) {
      title.textContent = `Actions effectuées : ${actionCategories[step]}`;
  }
}

function updateSelectedActions() {
  selectedActions = Array.from(document.querySelectorAll("#currentCategoryActions input[type='checkbox']:checked"))
    .map(input => input.value);
}

function showCategory(step) {
  categories.forEach((_, index) => {
    const categoryDiv = document.getElementById(`category-${index}`);
    if (categoryDiv) {
      categoryDiv.style.display = index === step ? "block" : "none";
    }
  });
  const title = document.getElementById("stepTitle");
  if(currentMode === "forward" && categories[step]) {
      title.textContent = `Étape ${step + 1} : ${categories[step]}`;
  }
}

function renderFactsForStep(step) { showCategory(step); }

function updateSelectedFacts() {
  selectedFacts = Array.from(document.querySelectorAll("input[type='checkbox']:checked"))
    .map(input => input.value);
}

function renderProgressIndicator() {
  const indicator = document.getElementById("progressIndicator");
  indicator.innerHTML = "";
  const steps = currentMode === "forward" ? categories.length : actionCategories.length;
  
  for (let i = 0; i < steps; i++) {
    const dot = document.createElement("div");
    dot.className = "progress-dot";
    
    if(currentMode === "forward") {
        dot.setAttribute("title", categories[i] || "");
    } else {
        dot.setAttribute("title", actionCategories[i] || "");
    }
    
    if (i < currentStep) dot.classList.add("completed");
    else if (i === currentStep) dot.classList.add("active");
    
    dot.addEventListener("click", () => jumpToStep(i));
    indicator.appendChild(dot);
  }
}

function jumpToStep(stepIndex) {
    if (currentMode === "forward") {
        updateSelectedFacts();
        currentStep = stepIndex;
        renderFactsForStep(currentStep);
    } else {
        updateSelectedActions();
        currentStep = stepIndex;
        showActionCategory(currentStep);
    }
    renderProgressIndicator();
}

function showScreen(screenId) {
  document.querySelectorAll(".screen").forEach(screen => {
    screen.classList.remove("active");
  });
  document.getElementById(screenId).classList.add("active");
}

function startForwardChaining() {
  currentMode = "forward";
  currentStep = 0;
  selectedFacts = [];
  document.querySelectorAll("input[type='checkbox']").forEach(cb => cb.checked = false);
  
  document.getElementById("factsStep").style.display = "block";
  document.getElementById("factsStep").classList.add("active");
  document.getElementById("goalStep").style.display = "none";
  document.getElementById("goalStep").classList.remove("active");
  document.getElementById("actionsStep").style.display = "none";
  document.getElementById("actionsStep").classList.remove("active");
  document.getElementById("btnSkip").style.display = "inline-block";
  
  renderProgressIndicator();
  renderFactsForStep(currentStep);
  showScreen("wizardScreen");
}

function startBackwardChaining() {
  currentMode = "backward";
  currentStep = 0;
  selectedActions = [];
  document.querySelectorAll("#currentCategoryActions input[type='checkbox']").forEach(cb => cb.checked = false);
  
  document.getElementById("factsStep").style.display = "none";
  document.getElementById("factsStep").classList.remove("active");
  document.getElementById("goalStep").style.display = "none";
  document.getElementById("goalStep").classList.remove("active");
  document.getElementById("actionsStep").style.display = "block";
  document.getElementById("actionsStep").classList.add("active");
  document.getElementById("btnSkip").style.display = "inline-block";
  
  loadActions().then(() => {
    renderProgressIndicator();
    showActionCategory(currentStep);
    showScreen("wizardScreen");
  });
}

async function runDiagnostic() {
  showScreen("loadingScreen");
  try {
    let payload;
    if (currentMode === "forward") {
      updateSelectedFacts();
      payload = { facts: selectedFacts, mode: "forward", goal: "" };
    } else {
      updateSelectedActions();
      if (selectedActions.length === 0) {
        alert("Veuillez sélectionner au moins une action effectuée");
        startBackwardChaining();
        return;
      }
      payload = { facts: [], mode: "backward", goal: "", "selected-actions": selectedActions };
    }
    
    const response = await fetch("/api/diagnostic", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(payload)
    });
    
    if (!response.ok) throw new Error(`Status: ${response.status}`);
    const result = await response.json();
    displayResults(result);
  } catch (error) {
    console.error("Erreur:", error);
    showScreen("resultsScreen");
    document.getElementById("actionsList").innerHTML = 
      `<div class="results-item">Erreur: ${error.message}</div>`;
  }
}

function displayResults(result) {
  showScreen("resultsScreen");
  const actionsList = document.getElementById("actionsList");
  actionsList.innerHTML = "";
  
  if (currentMode === "backward" && result.resolved_problems) {
    displayBackwardResults(result.resolved_problems);
    return;
  }
  
  const preciseActions = [];
  const generalActions = [];

  if (result.rule_conclusions && result.rule_conclusions.length > 0) {
    result.rule_conclusions.forEach(item => {
      const match = item.match(/ACTION_RECOMMANDEE\([^,]+,(.+)\)$/);
      if (match) {
        const message = match[1].trim();
        if (message.startsWith("PRECIS:")) {
          preciseActions.push(message.substring(7).trim());
        } else if (message.startsWith("GENERAL:")) {
          generalActions.push(message.substring(8).trim());
        } else {
          generalActions.push(message);
        }
      }
    });
  }
  
  if (preciseActions.length > 0) {
    const titlePrecis = document.createElement("div");
    titlePrecis.className = "results-section-title";
    titlePrecis.innerHTML = "Diagnostics très probables";
    actionsList.appendChild(titlePrecis);
    
    preciseActions.forEach(msg => {
      const div = document.createElement("div");
      div.className = "results-item results-precise";
      div.textContent = msg;
      actionsList.appendChild(div);
    });
  }
  
  if (generalActions.length > 0) {
    const titleGeneral = document.createElement("div");
    titleGeneral.className = "results-section-title results-section-general";
    titleGeneral.innerHTML = "Conseils généraux";
    actionsList.appendChild(titleGeneral);
    
    generalActions.forEach(msg => {
      const div = document.createElement("div");
      div.className = "results-item results-general";
      div.textContent = msg;
      actionsList.appendChild(div);
    });
  }
  
  if (preciseActions.length === 0 && generalActions.length === 0) {
    const div = document.createElement("div");
    div.className = "results-item";
    div.textContent = "Aucune action spécifique recommandée pour cette situation.";
    actionsList.appendChild(div);
  }
}

function displayBackwardResults(resolvedProblems) {
  const actionsList = document.getElementById("actionsList");
  
  if (!resolvedProblems || resolvedProblems.length === 0) {
    const div = document.createElement("div");
    div.className = "results-item";
    div.textContent = "Les actions sélectionnées ne correspondent à aucun problème connu dans notre base.";
    actionsList.appendChild(div);
    return;
  }
  
  const groupedByCategory = {};
  resolvedProblems.forEach(problem => {
    const category = problem[2] || 'Autre';
    if (!groupedByCategory[category]) {
      groupedByCategory[category] = [];
    }
    groupedByCategory[category].push(problem);
  });
  
  const mainTitle = document.createElement("div");
  mainTitle.className = "results-section-title";
  mainTitle.innerHTML = "Problèmes potentiellement résolus";
  actionsList.appendChild(mainTitle);
  
  const sortedCategories = Object.keys(groupedByCategory).sort();
  sortedCategories.forEach(category => {
    const categoryTitle = document.createElement("div");
    categoryTitle.className = "results-category-title";
    categoryTitle.textContent = category;
    actionsList.appendChild(categoryTitle);
    
    groupedByCategory[category].forEach(problem => {
      const div = document.createElement("div");
      div.className = "results-item results-resolved";
      div.textContent = problem[1];
      actionsList.appendChild(div);
    });
  });
}

document.getElementById("btnForward").addEventListener("click", startForwardChaining);
document.getElementById("btnBackward").addEventListener("click", startBackwardChaining);
document.getElementById("btnNext").addEventListener("click", () => {
  if (currentMode === "forward") {
    updateSelectedFacts();
    if (currentStep < categories.length - 1) {
      currentStep++;
      renderFactsForStep(currentStep);
      renderProgressIndicator();
    } else {
      runDiagnostic();
    }
  } else {
    updateSelectedActions();
    if (currentStep < actionCategories.length - 1) {
      currentStep++;
      showActionCategory(currentStep);
      renderProgressIndicator();
    } else {
      runDiagnostic();
    }
  }
});
document.getElementById("btnSkip").addEventListener("click", () => {
    if (currentMode === "forward") {
        updateSelectedFacts();
    } else {
        updateSelectedActions();
    }
    runDiagnostic();
});
document.getElementById("btnBack").addEventListener("click", () => {
  if (currentMode === "forward" && currentStep > 0) {
    currentStep--;
    renderFactsForStep(currentStep);
    renderProgressIndicator();
  } else if (currentMode === "backward" && currentStep > 0) {
    currentStep--;
    showActionCategory(currentStep);
    renderProgressIndicator();
  } else {
    showScreen("welcomeScreen");
  }
});
document.getElementById("btnNewDiagnosis").addEventListener("click", () => {
  showScreen("welcomeScreen");
});

const modal = document.getElementById("aboutModal");
const openModalBtn = document.getElementById("openModalBtn");
const closeModalBtn = document.querySelector(".close-modal");
openModalBtn.addEventListener("click", () => modal.style.display = "flex");
closeModalBtn.addEventListener("click", () => modal.style.display = "none");
window.addEventListener("click", (e) => { if (e.target === modal) modal.style.display = "none"; });

loadFacts();