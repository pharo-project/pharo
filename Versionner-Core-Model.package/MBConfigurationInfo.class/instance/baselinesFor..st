accessing
baselinesFor: aMetacelloProject
	"Return the list of  BaselineInfo for the configuration class passed as parameter"

	"The returned collection is ordered. Labeled baselines are first, numbered baselines are last"

	| existingBaselines numberedBaselines labeledBaselines |
	existingBaselines := aMetacelloProject versions select: [ :vrsn | vrsn blessing == #baseline ].
	existingBaselines := existingBaselines
		collect: [ :vrsn | 
			(MBBaselineInfo version: vrsn)
				configurationInfo: self;
				yourself ].
	numberedBaselines := existingBaselines select: [ :bInfo | ($0 to: $9) includes: bInfo name first ].
	labeledBaselines := existingBaselines copyWithoutAll: numberedBaselines.
	^ labeledBaselines asArray , numberedBaselines asArray reversed