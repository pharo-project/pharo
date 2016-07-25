converting
adaptToInteger: rcvr andCompare: selector 
	"If I am involved in comparison with an Integer.
	Default behaviour is to process comparison as any other selectors."
	^ self adaptToInteger: rcvr andSend: selector