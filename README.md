# microGalaxy 2025 Paper

This repository includes all the scripts written to produce figures in the paper along with the resulted figures

# Requirements

- Install [conda](https://conda.io/miniconda.html)

    ```
    $ make install-conda
    ```

- Create the conda environment

    ```
    $ make create-env
    ```

# Usage

## Retrieve citations

- Launch [Jupyter](https://jupyter.org/) to access the notebooks to generate graphs

    ```
    $ make run-jupyter
    ```

- Go to [http://localhost:8888](http://localhost:8888) (a page should open automatically in your browser)

## Update data

- Launch the dedicated script

    ```
    $ bash bin/get_data.sh
    ```
## Rebuild Figures

```{r}
Rscript bin/figure_2.R                   # Figure 2
Rscript bin/figure_3.R                   # Figure 3
 
Rscript bin/extended_data_figure_1.R     # Extended Data Figure 1
Rscript bin/extended_data_figure_2.R     # Extended Data Figure 2
Rscript bin/extended_data_figure_3.R     # Extended Data Figure 3
Rscript bin/extended_data_figure_4.R     # Extended Data Figure 4
Rscript bin/extended_data_figure_6.R     # Extended Data Figure 6
Rscript bin/extended_data_figure_7.R     # Extended Data Figure 7
```




