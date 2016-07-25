private
withoutPrimitiveTryNamedPrimitiveIn: aCompiledMethod for: aReceiver withArgs: arguments
"When using the debugger we want to run a method step by step. What what happens when we do a step into a CompiledMethod which has a primitive? If such a method is executed form outside the Debugger (normal scenario) the VM knows that such CompiledMethod has a primitive declaration and hence executes it. If it fails then it continues executing all the bytecode of the method. Otherwise, it just returns. 

Now, what is the problem with the Debugger? The problem is that if the primitive fail, we don't want that the VM directly executes all the remaining bytecodes of the method. Instead, we would like to go step by step witht he Debugger, just as happens with normal methods. 

To solve the mentioned problem, we use the following trick: We have the orignal compiled method (the one that has a primitive invokation), the receiver and the arguments. So the idea is to use a template compiled method that ONLY contains the primitive delcaration (it doesn't include all the smalltalk code after the primitive). #tryNamedPrimitiveTemplateMethod answers such a template method which looks like:

tryNamedPrimitive
	<primitive:'' module:''>
	^ Context primitiveFailToken'

 Since this method does not change its bytecodes for every invokation, we can reuse it for all methods with primitives. There are only 2 things we have to change in the template: the number of arguments and the primitive declaration (to use the correct primitive name and module name).

Then what we do is to run that compiled method with the receiver and arguments we have. The result is that we will be invoking almost the same original method but a slighly different version that does not have the smalltalk part after the primitive and that in contrast is sends #primitiveFailToken. If this method invokation does not fail, then the Debugger continues debugging the sender of the primitive method. In this case, the step in is the same as step over. If the primitive fails, then the debugger continues executing the smalltalk part after the primitive method. In this case, step in is a real step in.  "	
	| theMethod  |
	arguments size > 8 
		ifTrue: [ ^ self class primitiveFailToken ].
	theMethod := self class tryNamedPrimitiveTemplateMethod.
	theMethod prepareForSimulationWith: arguments size.
	theMethod == nil 
		ifTrue: [ ^ self class primitiveFailToken ].
	self setNamedPrimitiveInformationFrom: aCompiledMethod toMethod: theMethod.
	theMethod flushCache.
	
	^ theMethod valueWithReceiver: aReceiver arguments: arguments