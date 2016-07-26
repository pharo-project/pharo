public access
compile
	| cm |
	[ 	[	ast := self parse.
			self doSemanticAnalysis. 
			self callPlugins.  
		] 	on: OCSourceCodeChanged 
			do: 
			[  	:notification | 
				self source: notification newSourceCode. 
				notification retry. 
			]. 
		cm := ast generate: self compilationContext compiledMethodTrailer 
	]	on: SyntaxErrorNotification 
		do: [ :exception | 
			self compilationContext requestor
                ifNotNil: [
						self compilationContext requestor 
							notify: exception errorMessage , ' ->'
							at: exception location
							in: exception errorCode.
                    ^ self compilationContext failBlock value ]
                ifNil: [ exception pass ]].
	^cm