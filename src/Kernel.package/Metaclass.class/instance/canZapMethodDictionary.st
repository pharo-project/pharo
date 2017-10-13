testing
canZapMethodDictionary
	"Return true if it is safe to zap the method dictionary on #obsolete"
	self soleInstance == nil
		ifTrue:[^true]
		ifFalse:[^self soleInstance canZapMethodDictionary]