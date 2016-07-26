converting
adaptToFraction: rcvr andCompare: selector 
	"If I am involved in comparison with a Fraction.
	Default behaviour is to process comparison as any other selectors."
	^ self adaptToFraction: rcvr andSend: selector