accessing
methodDict
       "The method dictionary of a class can be nil when we want to use the #cannotInterpret: hook. Indeed when a class dictionary is nil, the VM sends the message cannotInterpret: to the receiver but starting the look up in the superclass of the class whose method dictionary was nil.
	 Now the system relies that when the message methodDict is sent to a class a method dictionary is returned. In order to prevent the complaints of tools and IDE unaware of this feature, we fool them by providing an empty MethodDictionary. This will hopefully work in most cases, but the tools will loose the ability to modify the behaviour of this behavior. The user of #cannotInterpret: should be aware of this."

       methodDict ifNil: [^ MethodDictionary new ].
       ^ methodDict