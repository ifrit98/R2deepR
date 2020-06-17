# R2deepR


[![Build Status](https://travis-ci.org/joemccann/dillinger.svg?branch=master)](https://travis-ci.org/joemccann/dillinger)

R2deepR is an R package designed to make the lives of Deep Learning practitioners a little more conveneint.

## Features

### Feature Scaling
Standardization

![alt text](https://github.com/ifrit98/R2deepR/raw/master/assets/standardize.png "")

![alt text](https://github.com/ifrit98/R2deepR/raw/master/assets/standard-scale.png "")

Minmax Normalization

![alt text](https://github.com/ifrit98/R2deepR/raw/master/assets/minmax.png "")

![alt text](https://github.com/ifrit98/R2deepR/raw/master/assets/minmax-scaler.png "")

Unit Vector Scaling

![alt text](https://github.com/ifrit98/R2deepR/raw/master/assets/unit-scaler.png "")


### Custom callbacks 
- lr_history
- save_optimizer_weights

### Fully Preprocessed keras datasets
Convenience functions to return fully preprocessed keras datasets as either numpy arrays or tensorflow-dataset objects.
- mnist
- imdb

### Novel activation functions
![alt text](https://github.com/ifrit98/R2deepR/raw/master/assets/activations2.png "")

- [gelu](https://arxiv.org/pdf/1606.08415v3.pdf)
- [brelu](https://arxiv.org/pdf/1709.04054.pdf)
- [belu](https://arxiv.org/pdf/1709.04054.pdf)


![alt text](https://github.com/ifrit98/R2deepR/raw/master/assets/nalu.png "")
- [nalu](https://arxiv.org/pdf/1808.00508.pdf)

### Novel layers
- Residual
- GLU
- PRU
- Antirectifier
- Autoencoder

### Learning Curve Plots
- training set size
- batch size
- learning rate

### Learning rate range test (see: Smith et al.)

### One Cycle Learning Rate Policy (see: Smith et al.)

### Evalation tools
- F1 score for binary classifiers

### Assorted utility and convenience functions

### Weight activation curves (WIP)
