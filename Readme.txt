I.
This package of R scripts provides tools to analyze time series data using Symbolic Aggregate approXimation (SAX), with
particular focus on the application domain of limnology. The approaches are based on the papers written by Eamonn Keogh's
research group at University of California, Riverside. Corresponding papers are cited within scripts.

II.
License: The scripts are developed by Indiana University and University of Wisconsin at Madison, under GNU General Public License.

III.
Overview:

We discribe the scripts based on their functionality as group and briefly introduce each script. You can find detailed comments
in each script which can help you understand the script easily.

##########################
Preprocessing
##########################

a) time series alignment:
Since time series may have different time resolution and timestamps when being compared, we need to align time series first.

We provide following scripts to fulfill this goal:

alignTSbyInterpolation.r: align time series by linear interpolation or spline interpolation

alignTSbyDTW.r: align time series by Dynamic Time Warping (DTW)

alignSAXbyDTW.r: align time series of SAX representation by Dynamic Time Warping (DTW). Note this is different from 'alignTSbyDTW.r'
which works against raw time series. Moreover, 'alignSAXbyDTW' uses SAX distance to measure SAX characters rather than Euclidean 
distance as used by 'alignTSbyDTW.r'.

b) time series slicing

As suggested in Eamonn Keogh's paper, oftentimes it is meaningless to directly transform a long raw time series into a single SAX
string, instead, we slicing it into multiple subsequences each of which is in turn symbolized as a SAX word/string.

timeseries2Subseqs.r: transform a raw time series into subsequences with an overlapped or non-overlapped sliding window.

######################################
Time series to symbolic representation
######################################

a) SAX representation

timeseries2Symbol.r: transform a time series subsequence into a SAX word. As mentioned earlier, normally we should not directly apply
this function to a long raw time series.

b) Time Series Bitmap (TSB)
Time series bitmap (TSB) can be regarded as a signature of a time series (can be the long raw time series) based on the SAX, in essence,
TSB is built by using a sliding window to slide the whole time series. Each slicing window will be transformed to a SAX word, which is
in turned chunked as 'subwords'. The TSB is a representation that reflects the frequencies of 'subwords', in the form of a normalized
numerical matrix.

timeseries2TSB.r: transform a time series into a TSB.

tsb2Img.r: visualize a TSB matrix as an image, like heatmap.

######################################
Distance measure
######################################

minDist.r: measure the distance of two SAX strings

matrixDist.r: measure the distance of two TSB matrices

###############################################
Knowledge discovery within a single time series
###############################################

a) find motifs:

findMotif.r: find motifs (most frequent subsequences/patterns) within a time series

b) anomaly detection:

anomalyDetectionTSB.r: detect anomaly based on TSB representation

anomalyDetectionCDM.r: detect anomaly based on SAX representation and on Compression-Based Dissimilarity Measure (CDM)

anmolyDetectionWCAD.r: detect anomaly based on SAX representation and CDM. Compared to 'anomalyDetectionCDM.r', this 
approach utilizes multiple windows.

################################################
Knowledge discovery amongst multiple time series
################################################

a) clustering

hclusteringSAX.r: hierarchical clustering based on SAX representation

hclusteringTSB.r: hierarchical clustering based on TSB representation

kmeansTSB.r: kmeans clustering based on TSB representation

b) classification

knnSAX.r: k nearest neighbor classification based on SAX representation

knnTSB.r: k nearest neighbor classification based on TSB representation

IV.
For any questions, please contact: Guangchen Ruan, gruan@indian.edu