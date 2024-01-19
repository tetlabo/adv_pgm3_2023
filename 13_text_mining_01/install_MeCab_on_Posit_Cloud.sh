#!/bin/bash

cd $HOME
wget -Omecab-0.996.tar.gz 'https://drive.google.com/uc?export=download&id=0B4y35FiV1wh7cENtOXlicTFaRUE&confirm=t'
wget -Omecab-ipadic-2.7.0-20070801.tar.gz 'https://drive.google.com/uc?export=download&id=0B4y35FiV1wh7MWVlSDBCSXZMTXM'

tar xzvf mecab-0.996.tar.gz
cd mecab-0.996
./configure --enable-utf8-only --prefix=$HOME/usr/local
make
make install

cd
export PATH=$PATH:$HOME/usr/local/bin

tar xzvf mecab-ipadic-2.7.0-20070801.tar.gz
cd mecab-ipadic-2.7.0-20070801
./configure --with-charset=utf8 --prefix=$HOME/usr/local
make
make install

cd
cp $HOME/usr/local/etc/mecabrc $HOME/.mecabrc
