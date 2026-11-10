#!/bin/bash
# NOTE this does need to be bash not fish, if working interactively

# tidy not used to not used
model=$(<../R/utils/modelchoice.txt)
echo "$model"

# TODO if notused not there create

# -------- important for TME team to have updated diagnostics

# copy plotting data into shared part of repo
orig="../outdata/KO.${model}.Rdata"
tgt="../prevplots/KO.${model}.Rdata"
echo "Copying $orig to $tgt"
cp $orig $tgt

cp ../data/RPD.Rdata ../prevplots

# make a copy of rhofnr in a synced place also
cp ../data/rhofnr.Rdata ../prevplots/rhofnr.Rdata

# -------- below here is tidying up unused plots if applicable
cd ../plots

# move out unused plots
for i in `ls r.*`; do
    if [[ $i != *"$model"* ]];then
       mv $i notused/;
       echo $i;
    fi;
done;

# move out unused timings
for i in `ls t.*`; do
    if [[ $i != *"$model"* ]];then
       mv $i notused/;
       echo $i;
    fi;
done;

# move out unused CV timings
for i in `ls t.*`; do
    if [[ $i =~ ^.*"$model"[A-Z]{3}\.txt$ ]];then # regex match versions with iso3 code on end
       mv $i notused/;
       echo $i;
    fi;
done;



# move out unused timings
for i in `ls tabs.*`; do
    if [[ $i != *"$model"* ]];then
       mv $i notused/;
       echo $i;
    fi;
done;

# move out unused timings
for i in `ls prop.*`; do
    if [[ $i != *"$model"* ]];then
       mv $i notused/;
       echo $i;
    fi;
done;

# move out unused timings
for i in `ls HBC30*`; do
    if [[ $i != *"$model"* ]];then
       mv $i notused/;
       echo $i;
    fi;
done;


# move out unused timings
for i in `ls tot.*`; do
    if [[ $i != *"$model"* ]];then
       mv $i notused/;
       echo $i;
    fi;
done;

