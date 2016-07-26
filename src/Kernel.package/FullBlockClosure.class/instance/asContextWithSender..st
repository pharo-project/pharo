private
asContextWithSender: aContext
	"Inner private support method for evaluation.  Do not use unless you know what you're doing."

	^(Context newForMethod: self compiledBlock)
		setSender: aContext
		receiver: self receiver
		method: self compiledBlock
		closure: self
		startpc: self startpc;
		privRefresh