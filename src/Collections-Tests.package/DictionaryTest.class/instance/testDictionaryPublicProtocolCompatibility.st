tests
testDictionaryPublicProtocolCompatibility
	"Tests that other dictionaries and their classes respond to the messages
	in the public protocols (ignoring extensions, private, printing, copying,
	and comparing) that Dictionary and its metaclass implement."

	| dict |
	self collectionClass == Dictionary
		ifTrue: [ ^ self ].
	dict := self emptyDict.
	{(dict -> Dictionary).
	(dict class -> Dictionary class)}
		do: [ :assoc | 
			assoc value protocols
				reject: [ :protocol | 
					#('private' 'print' 'copy' 'compar' '*')
						anySatisfy: [ :each | protocol asString beginsWith: each ] ]
				thenDo: [ :protocol | 
					(assoc value selectorsInProtocol: protocol)
						do: [ :each | self assert: (assoc key respondsTo: each) ] ] ]