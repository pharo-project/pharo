private
asContextWithSender: aContext
	"Inner private support method for evaluation.  Do not use unless you know what you're doing."

	^(Context newForMethod: outerContext method)
		setSender: aContext
		receiver: outerContext receiver
		method: outerContext method
		closure: self
		startpc: startpc;
		privRefresh