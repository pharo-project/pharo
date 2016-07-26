private-extensions-methodcontext
cleanCopyForMethodContext
	<flExtensionOf: #MethodContext selector: #cleanCopy isSource: true>
	^ 'cleanCopy 

	^ self class 
		sender: nil 
		receiver: receiver 
		method: method
		arguments: #()'