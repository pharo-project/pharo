initialize-release
name: varName key: objRef index: i type: type
	"Only used for initting global (litInd) variables"
	^self name: varName key: objRef code: (self code: (index := i) type: type)