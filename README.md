# mldc2020

Code to fit my [Mercado Libre Data Challenge](https://ml-challenge.mercadolibre.com/) 
2020 submission (7th place).

## Run

_Tested on a t2.xlarge AWS EC2 instance._

Create a new conda environment with:

```shell
conda create -n r-mldc2020 r-base=3.6.1 r-essentials=3.6.0
```

Then activate the environment and run:

```shell
Rscript download.R && Rscript main.R
```

Submission will be written to `data/submission.csv`.

## About

_This section is a bit superficial. As of now, you can understand the model 
better through the `infer` function in the `functions/infer.R` script._

The model was made from scratch, without any common recsys or classification 
algorithms. It's basically a weighted average followed by some sorting rules, 
and inference is very fast (and it could easily be faster with parallelization). 
The model roughly described by the pseudocode below:

```
for each user history do:
  for each distinct item do:
    item frequency = normalized count of item views;
    item recency = normalized rank of items ordered by ascending date;
    item popularity = normalized count of item sales in other histories;
    item score = weighted mean of features;
  end
  top category = category of highest scoring item;
  relevant items = subset of items with at least one sale in other histories;
  recommendations = relevant items ranked by score;
  if number of recommendations is less than 10 do:
    recommendations = concatenate recommendations with top category items ranked by number of sales in other histories;
  end
  return recommendations
end
```

During pre-processing, fuzzy string matching is used to associate searches to 
item ids.

## Tune

There are only three parameters: weights for the weighted mean of frequency, recency and popularity.
By default, the `main.R` script fits the model with the parameters in `data/best_params.csv`. To tune them, first run:

```r
# This installation is separated from the others because this package 
# imports too many dependencies. It might be a good idea to substitute it
# in a future iteration.
remotes::install_version('ParBayesianOptimization', '1.2.1')
```

Then change the `train` variable in the `main.R` script (line 30).

```r
train <- TRUE # FALSE is the default
```

Afterwards, run the script. It will test new parameters and overwrite the `data/best_params.csv` file with the best set. Then, change the `train` variable back to `FALSE` and rerun the script to generate a submission with the newly tuned parameters.
