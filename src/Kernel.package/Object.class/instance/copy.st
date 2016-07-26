copying
copy
	"Answer another instance just like the receiver. Subclasses typically override postCopy; they typically do not override shallowCopy. Copy is a template method in the sense of Design Patterns. So do not override it. Override postCopy instead. Pay attention that normally you should call postCopy of your superclass too."

	^self shallowCopy postCopy