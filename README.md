<!-- Badges -->

[![Shiny App](https://img.shields.io/badge/Shiny-online-brightgreen)](https://pavlopoulos-lab-services.org/shiny/app/nap)
[![GitHub Repo](https://img.shields.io/badge/GitHub-PavlopoulosLab%2FNAP-blue)](https://github.com/PavlopoulosLab/NAP)

# NAP: Network Analysis Profiler

> A mediator for shaking hands between graph theory and systems biology.

---

## 📖 Table of Contents

1. [Overview](#overview)
2. [Key Features](#key-features)
3. [Installation](#installation)
4. [Usage](#usage)
5. [Publications](#publications)
6. [License](#license)

---

## 📝 Overview

NAP is a Shiny-based web application designed to profile the topology of medium-scale networks (up to a few thousand edges) and facilitate direct comparison through an intuitive interface. It bridges graph-theoretical metrics with systems biology needs, enabling users to explore, rank, and compare network topology seamlessly.

---

## 🚀 Key Features

* **Topological Exploration**: Compute and visualize metrics such as degree, betweenness, closeness, clustering coefficient and more.
* **Node & Edge Ranking**: Rank elements by any computed metric and export results as tables or plots.
* **Multi-Network Comparison**: Load multiple networks simultaneously to compare topological properties side-by-side.
* **Feature Distributions**: Plot distributions of any topological feature across networks.
* **Feature Correlation**: Generate scatter plots of one metric against another to uncover relationships.
* **Dynamic & Static Layouts**: Visualize networks using layouts like Fruchterman–Reingold, Kamada–Kawai, circular, grid, and interactive 3D layers.

---

## 🛠 Installation

### Clone & Run Locally

```bash
git clone https://github.com/PavlopoulosLab/NAP.git
cd NAP
```

```r
# In R console or RStudio:
source("install_dependencies.R")   # install required R packages
shiny::runApp(port = 3838)         # launch the app
```

### Online Demo

Access NAP directly at: [https://pavlopoulos-lab-services.org/shiny/app/nap](https://pavlopoulos-lab-services.org/shiny/app/nap)

---

## 💻 Usage

1. **Upload** one or more network edge-list files (tab-delimited).
2. **Select** topological metrics to compute and visualize.
3. **Compare** metric distributions and rankings across uploaded networks.
4. **Explore** interactive network views, adjust layouts, and export publication-ready figures.

---

## 📚 Publications

* **The Network Analysis Profiler (NAP v2.0): A web tool for visual topological comparison between multiple networks**
  Koutrouli M., Theodosiou T., Iliopoulos I., Pavlopoulos G.A.
  *EMBnet.journal*, 2021 May;26\:e943.
  doi: [10.14806/ej.26.0.943](https://doi.org/10.14806/ej.26.0.943)

* **NAP: The Network Analysis Profiler, a web tool for easier topological analysis and comparison of medium-scale biological networks**
  Theodosiou T., Efstathiou G., Papanikolaou N., Kyrpides N.C., Bagos P.G., Iliopoulos I., Pavlopoulos G.A.
  *BMC Research Notes*, 2017;10:278.
  doi: [10.1186/s13104-017-2607-8](https://doi.org/10.1186/s13104-017-2607-8)
  PMID: [28705239](https://pubmed.ncbi.nlm.nih.gov/28705239/)

---

## 📄 License

This project is released under the **MIT License**. See the [LICENSE](LICENSE) file for details.
