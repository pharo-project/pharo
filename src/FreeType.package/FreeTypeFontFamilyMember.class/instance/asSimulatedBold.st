copying
asSimulatedBold
	^self copy 
		weightValue: LogicalFont weightBold;
		styleName: (fileInfo styleNameWithWeightForcedToBe: 'Bold');
		simulated: true;
		yourself