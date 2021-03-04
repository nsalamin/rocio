#!/bin/bash

ls RandTrees/*.tree | while read tree; do
	Rscript --vanilla simulCoevMixture.R ${tree} 
done