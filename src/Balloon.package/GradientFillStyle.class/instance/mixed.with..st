converting
mixed: fraction with: aColor
	^self copy colorRamp: (colorRamp collect:[:assoc| assoc key -> (assoc value mixed: fraction with: aColor)])