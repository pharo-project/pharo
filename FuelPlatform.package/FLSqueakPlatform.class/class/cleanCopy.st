private-extensions-methodcontext
cleanCopy 
	<flExtensionOf: #MethodContext isSource: true>
	^ 'cleanCopy 
	^ self class 
		sender: nil 
		receiver: receiver 
		method: method
		arguments: #()'