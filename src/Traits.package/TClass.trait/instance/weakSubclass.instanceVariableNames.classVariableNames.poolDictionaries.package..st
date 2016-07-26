subclass creation - weak
weakSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s package: cat
	"This is the standard initialization message for creating a new class as a subclass of an existing class (the receiver) in which the subclass is to have weak indexable pointer variables."

	^self weakSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat