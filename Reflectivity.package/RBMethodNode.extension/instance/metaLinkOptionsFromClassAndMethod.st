*Reflectivity
metaLinkOptionsFromClassAndMethod 
	| options |
	options := Set new.
	self methodClass 
		compiledMethodAt: #metaLinkOptions 
		ifPresent: [:method | options parseOptions: ((method valueWithReceiver: nil arguments: #()) asDictionary at:  self selector ifAbsent: [#()])] 
		ifAbsent: [].
	"set meta link options defined per method"
	(self pragmas select: [ :pragma | pragma selector == #metaLinkOptions:]) 
		do: [ :pragma | (pragma asPragma setKeyword: #parseOptions:) sendTo: options ].
	^options.
		
	