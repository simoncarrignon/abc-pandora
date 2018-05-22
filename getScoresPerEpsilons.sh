#!/bin/bash

echo "score,epsilon" > $1

for i in rerun-tenthousand*/ ; do
    fn="${i%/}"
    epsilon="${fn/rerun-tenthousand-/}"
    echo $epsilon
    cat rerun-tenthousand-"$epsilon"/*/score.txt > tmp 
    cat tmp | awk -v eps=$epsilon '{print $1","eps}' >> $1 
done

