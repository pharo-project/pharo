Class {
	#name : 'ReflectivityOnStackTest',
	#superclass : 'TestCase',
	#instVars : [
		'tag',
		'link',
		'link2',
		'tag2'
	],
	#category : 'Reflectivity-Tests-Base',
	#package : 'Reflectivity-Tests',
	#tag : 'Base'
}

{ #category : 'accessing' }
ReflectivityOnStackTest >> tag [

	^ tag
]

{ #category : 'accessing' }
ReflectivityOnStackTest >> tag2 [

	^ tag2
]

{ #category : 'accessing' }
ReflectivityOnStackTest >> tag2: anObject [

	tag2 := anObject
]

{ #category : 'tagging' }
ReflectivityOnStackTest >> tag2Exec [

	tag2 := #yes
]

{ #category : 'accessing' }
ReflectivityOnStackTest >> tag: anObject [

	tag := anObject
]

{ #category : 'tagging' }
ReflectivityOnStackTest >> tagExec [
	tag := #yes
]

{ #category : 'running' }
ReflectivityOnStackTest >> tearDown [

	link ifNotNil: [ link uninstall ].
	link2 ifNotNil: [ link2 uninstall ].
	super tearDown
]

{ #category : 'tests' }
ReflectivityOnStackTest >> testExecuteWithWaiting [
	| instance process |
	instance := ReflectivityExampleOnStack new.
	
	process := [ instance exampleMethodWaitingOnSemaphore ] fork.

	"here check that the method is on the stack"	
	0.1 seconds wait.
	self assert: process suspendedContext sender method equals: ReflectivityExampleOnStack>>#exampleMethodWaitingOnSemaphore.
	
	"process should be running"
	self deny: process isTerminated.
	instance continue.
	0.1 seconds wait.
	self assert: process isTerminated.

]

{ #category : 'tests' }
ReflectivityOnStackTest >> testExecuteWithWaitingWithAfterLinkInstalledAfterExecutionAtInstructionChangeInterruptedMethodManually [

	| instance messageNode process |
	instance := ReflectivityExampleOnStack new.

	process := [ instance exampleMethodWaitingOnSemaphoreDirectly ] fork.
	0.1 seconds wait.

	"If we install the link before the instruction the program stopped to, it should not be executed"
	messageNode := (ReflectivityExampleOnStack
	                >> #exampleMethodWaitingOnSemaphoreDirectly) ast
		               sendNodes second.
	link := MetaLink new
		        metaObject: self;
		        control: #after;
		        selector: #tagExec.
	messageNode link: link.
	self assert: messageNode hasMetalink.
	self
		assert: (ReflectivityExampleOnStack
			 >> #exampleMethodWaitingOnSemaphoreDirectly) class
		equals: ReflectiveMethod.
	self assert: tag isNil.

	self
		assert: (ReflectivityExampleOnStack
			 >> #exampleMethodWaitingOnSemaphoreDirectly) class
		equals: ReflectiveMethod.

	"make sure to create the compiledMethod"
	(ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphoreDirectly)
		compileAndInstallCompiledMethod.

	process suspendedContext method:
		ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphoreDirectly.

	"here check that the method is on the stack"

	self
		assert: process suspendedContext method selector
		equals: #exampleMethodWaitingOnSemaphoreDirectly.
	"process should be running"
	self deny: process isTerminated.

	instance continue.
	0.1 seconds wait.
	self assert: process isTerminated.


	"It got executed!!"
	self assert: tag notNil
]

{ #category : 'tests' }
ReflectivityOnStackTest >> testExecuteWithWaitingWithAfterLinkInstalledAfterExecutionAtInstructionChangeMethodManually [

	| instance messageNode process |
	instance := ReflectivityExampleOnStack new.

	process := [ instance exampleMethodWaitingOnSemaphore ] fork.
	0.1 seconds wait.

	"If we install the link before the instruction the program stopped to, it should not be executed"
	messageNode := (ReflectivityExampleOnStack
	                >> #exampleMethodWaitingOnSemaphore) ast sendNodes
		               second.
	link := MetaLink new
		        metaObject: self;
		        control: #after;
		        selector: #tagExec.
	messageNode link: link.
	self assert: messageNode hasMetalink.
	self
		assert:
		(ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphore)
			class
		equals: ReflectiveMethod.
	self assert: tag isNil.

	self
		assert:
		(ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphore)
			class
		equals: ReflectiveMethod.

	"make sure to create the compiledMethod"
	(ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphore)
		compileAndInstallCompiledMethod.

	process suspendedContext sender method:
		ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphore.

	"here check that the method is on the stack"

	self
		assert: process suspendedContext sender method selector
		equals: #exampleMethodWaitingOnSemaphore.
	"process should be running"
	self deny: process isTerminated.

	instance continue.
	0.1 seconds wait.
	self assert: process isTerminated.


	"It got executed!!"
	self assert: tag notNil
]

{ #category : 'tests' }
ReflectivityOnStackTest >> testExecuteWithWaitingWithBeforeLinkInstalledAfterExecutionAtInstructionChangeInterruptedMethodManually [

	| instance messageNode process |
	instance := ReflectivityExampleOnStack new.

	process := [ instance exampleMethodWaitingOnSemaphoreDirectly ] fork.
	0.1 seconds wait.

	"If we install the link before the instruction the program stopped to, it should not be executed"
	messageNode := (ReflectivityExampleOnStack
	                >> #exampleMethodWaitingOnSemaphoreDirectly) ast
		               sendNodes second.
	link := MetaLink new
		        metaObject: self;
		        control: #before;
		        selector: #tagExec.
	messageNode link: link.
	self assert: messageNode hasMetalink.
	self
		assert: (ReflectivityExampleOnStack
			 >> #exampleMethodWaitingOnSemaphoreDirectly) class
		equals: ReflectiveMethod.
	self assert: tag isNil.

	self
		assert: (ReflectivityExampleOnStack
			 >> #exampleMethodWaitingOnSemaphoreDirectly) class
		equals: ReflectiveMethod.

	"make sure to create the compiledMethod"
	(ReflectivityExampleOnStack
	 >> #exampleMethodWaitingOnSemaphoreDirectly)
		compileAndInstallCompiledMethod.

	process suspendedContext method:
		ReflectivityExampleOnStack
		>> #exampleMethodWaitingOnSemaphoreDirectly.

	"here check that the method is on the stack"

	self
		assert: process suspendedContext method selector
		equals: #exampleMethodWaitingOnSemaphoreDirectly.
	"process should be running"
	self deny: process isTerminated.

	instance continue.
	0.1 seconds wait.
	self assert: process isTerminated.


	"It has not been executed!!"
	self assert: tag isNil
]

{ #category : 'tests' }
ReflectivityOnStackTest >> testExecuteWithWaitingWithBeforeLinkInstalledAfterExecutionAtInstructionChangeMethodManually [

	| instance messageNode process |
	instance := ReflectivityExampleOnStack new.

	process := [ instance exampleMethodWaitingOnSemaphore ] fork.
	0.1 seconds wait.

	"If we install the link before the instruction the program stopped to, it should not be executed"
	messageNode := (ReflectivityExampleOnStack
	                >> #exampleMethodWaitingOnSemaphore) ast sendNodes
		               second.
	link := MetaLink new
		        metaObject: self;
		        control: #before;
		        selector: #tagExec.
	messageNode link: link.
	self assert: messageNode hasMetalink.
	self
		assert:
		(ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphore)
			class
		equals: ReflectiveMethod.
	self assert: tag isNil.

	self
		assert:
		(ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphore)
			class
		equals: ReflectiveMethod.

	"make sure to create the compiledMethod"
	(ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphore)
		compileAndInstallCompiledMethod.

	process suspendedContext sender method:
		ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphore.

	"here check that the method is on the stack"

	self
		assert: process suspendedContext sender method selector
		equals: #exampleMethodWaitingOnSemaphore.
	"process should be running"
	self deny: process isTerminated.

	instance continue.
	0.1 seconds wait.
	self assert: process isTerminated.


	"It has not been executed!!"
	self assert: tag isNil
]

{ #category : 'tests' }
ReflectivityOnStackTest >> testExecuteWithWaitingWithLinkInstalledAfterExecution [
	| instance messageNode process |
	instance := ReflectivityExampleOnStack new.
	
	
	
	process := [ instance exampleMethodWaitingOnSemaphore  ] fork.
	0.1 seconds wait.
	"If we install a link after we execute, we expect it to not matter for this test, as the new code will not be executed"
	messageNode := (ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphore) ast sendNodes third.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec.
	messageNode link: link.
	self assert: messageNode hasMetalink.
	self assert: (ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphore) class equals: ReflectiveMethod.
	self assert: tag isNil.

	"here check that the method is on the stack"	
	
	self assert: process suspendedContext sender method selector equals: #exampleMethodWaitingOnSemaphore.
	"process should be running"
	self deny: process isTerminated.
	instance continue.
	0.1 seconds wait.
	self assert: process isTerminated.
	
	
	"here the tag should not be nil, the metalink should have been executed."
	self deny: tag notNil.
	
	"but if we execute again, it workd"

	process := [ instance exampleMethodWaitingOnSemaphore ] fork.
	instance continue.
	0.1 seconds wait.
	self assert: tag notNil.

]

{ #category : 'tests' }
ReflectivityOnStackTest >> testExecuteWithWaitingWithLinkInstalledAfterExecutionBeforeInstructionChangeInterruptedMethodManually [

	| instance messageNode process |
	instance := ReflectivityExampleOnStack new.

	process := [ instance exampleMethodWaitingOnSemaphoreDirectly ] fork.
	0.1 seconds wait.
	"If we install a link before the instruction the program stopped to, the link should not be executed"
	messageNode := (ReflectivityExampleOnStack
	                >> #exampleMethodWaitingOnSemaphoreDirectly) ast
		               sendNodes first.
	link := MetaLink new
		        metaObject: self;
		        selector: #tagExec.
	messageNode link: link.
	self assert: messageNode hasMetalink.
	self
		assert: (ReflectivityExampleOnStack
			 >> #exampleMethodWaitingOnSemaphoreDirectly) class
		equals: ReflectiveMethod.
	self assert: tag isNil.


	"Here we could do the on stack replacement manually"

	self
		assert: (ReflectivityExampleOnStack
			 >> #exampleMethodWaitingOnSemaphoreDirectly) class
		equals: ReflectiveMethod.

	"make sure to create the compiledMethod"
	(ReflectivityExampleOnStack
	 >> #exampleMethodWaitingOnSemaphoreDirectly)
		compileAndInstallCompiledMethod.

	process suspendedContext method:
		ReflectivityExampleOnStack
		>> #exampleMethodWaitingOnSemaphoreDirectly.

	"here check that the method is on the stack"

	self
		assert: process suspendedContext method selector
		equals: #exampleMethodWaitingOnSemaphoreDirectly.
	"process should be running"
	self deny: process isTerminated.
	
	instance continue.
	0.1 seconds wait.
	self assert: process isTerminated.


	"It was not executed!!"
	self assert: tag isNil
]

{ #category : 'tests' }
ReflectivityOnStackTest >> testExecuteWithWaitingWithLinkInstalledAfterExecutionBeforeInstructionChangeMethodManually [

	| instance messageNode process |
	instance := ReflectivityExampleOnStack new.

	process := [ instance exampleMethodWaitingOnSemaphore ] fork.
	0.1 seconds wait.

	"If we install the link before the instruction the program stopped to, it should not be executed"
	messageNode := (ReflectivityExampleOnStack
	                >> #exampleMethodWaitingOnSemaphore) ast sendNodes
		               first.
	link := MetaLink new
		        metaObject: self;
		        selector: #tagExec.
	messageNode link: link.
	self assert: messageNode hasMetalink.
	self
		assert:
		(ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphore)
			class
		equals: ReflectiveMethod.
	self assert: tag isNil.

	self
		assert:
		(ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphore)
			class
		equals: ReflectiveMethod.

	"make sure to create the compiledMethod"
	(ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphore)
		compileAndInstallCompiledMethod.

	process suspendedContext sender method:
		ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphore.

	"here check that the method is on the stack"

	self
		assert: process suspendedContext sender method selector
		equals: #exampleMethodWaitingOnSemaphore.
	"process should be running"
	self deny: process isTerminated.

	instance continue.
	0.1 seconds wait.
	self assert: process isTerminated.


	"It has not been executed!!"
	self assert: tag isNil
]

{ #category : 'tests' }
ReflectivityOnStackTest >> testExecuteWithWaitingWithLinkInstalledAfterExecutionChangeInterruptedMethodManually [

	| instance messageNode process |
	instance := ReflectivityExampleOnStack new.

	process := [ instance exampleMethodWaitingOnSemaphoreDirectly ] fork.
	0.1 seconds wait.
	"If we install a link after we execute, we expect it to not matter for this test, as the new code will not be executed"
	messageNode := (ReflectivityExampleOnStack
	                >> #exampleMethodWaitingOnSemaphoreDirectly) ast
		               sendNodes third.
	link := MetaLink new
		        metaObject: self;
		        selector: #tagExec.
	messageNode link: link.
	self assert: messageNode hasMetalink.
	self
		assert: (ReflectivityExampleOnStack
			 >> #exampleMethodWaitingOnSemaphoreDirectly) class
		equals: ReflectiveMethod.
	self assert: tag isNil.


	"Here we could do the on stack replacement manually"

	self
		assert: (ReflectivityExampleOnStack
			 >> #exampleMethodWaitingOnSemaphoreDirectly) class
		equals: ReflectiveMethod.

	"make sure to create the compiledMethod"
	(ReflectivityExampleOnStack
	 >> #exampleMethodWaitingOnSemaphoreDirectly)
		compileAndInstallCompiledMethod.

	process suspendedContext method:
		ReflectivityExampleOnStack
		>> #exampleMethodWaitingOnSemaphoreDirectly.

	"here check that the method is on the stack"

	self
		assert: process suspendedContext method selector
		equals: #exampleMethodWaitingOnSemaphoreDirectly.
	"process should be running"
	self deny: process isTerminated.
	
	instance continue.
	0.1 seconds wait.
	self assert: process isTerminated.


	"It got executed!!"
	self assert: tag notNil
]

{ #category : 'tests' }
ReflectivityOnStackTest >> testExecuteWithWaitingWithLinkInstalledAfterExecutionChangeMethodManually [
	| instance messageNode process |
	instance := ReflectivityExampleOnStack new.
	
	
	
	process := [ instance exampleMethodWaitingOnSemaphore  ] fork.
	0.1 seconds wait.
	"If we install a link after we execute, we expect it to not matter for this test, as the new code will not be executed"
	messageNode := (ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphore) ast sendNodes third.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec.
	messageNode link: link.
	self assert: messageNode hasMetalink.
	self assert: (ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphore) class equals: ReflectiveMethod.
	self assert: tag isNil.


	"Here we could do the on stack replacement manually"
	
	self assert:	 (ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphore) class equals: ReflectiveMethod.
	
	"make sure to create the compiledMethod"
	(ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphore) compileAndInstallCompiledMethod.
	
	process suspendedContext sender method: (ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphore).
	
	"here check that the method is on the stack"	
	
	self assert: process suspendedContext sender method selector equals: #exampleMethodWaitingOnSemaphore.
	"process should be running"
	self deny: process isTerminated.

	instance continue.
	0.1 seconds wait.
	self assert: process isTerminated.
	
	
	"It got executed!!"
	self assert: tag notNil.

]

{ #category : 'tests' }
ReflectivityOnStackTest >> testExecuteWithWaitingWithLinkInstalledAfterExecutionOfMethodAfterExecutionOfBlockAfterCreationOfBlockChangeMethodManually [

	| instance messageNode process |
	instance := ReflectivityExampleOnStack new.



	process := [ instance exampleMethodWaitingOnSemaphoreInBlock ] fork.
	0.1 seconds wait.
	"If we install a link after we execute, we expect it to not matter for this test, as the new code will not be executed"
	messageNode := (ReflectivityExampleOnStack
	                >> #exampleMethodWaitingOnSemaphoreInBlock) ast
		               blockNodes first assignmentNodes second.
	link := MetaLink new
		        metaObject: self;
		        selector: #tagExec.
	messageNode link: link.
	self assert: messageNode hasMetalink.
	self
		assert:
			(ReflectivityExampleOnStack
			 >> #exampleMethodWaitingOnSemaphoreInBlock) class
		equals: ReflectiveMethod.
	self assert: tag isNil.


	"Here we could do the on stack replacement manually"

	self
		assert:
			(ReflectivityExampleOnStack
			 >> #exampleMethodWaitingOnSemaphoreInBlock) class
		equals: ReflectiveMethod.

	"make sure to create the compiledMethod"
	(ReflectivityExampleOnStack
	 >> #exampleMethodWaitingOnSemaphoreInBlock)
		compileAndInstallCompiledMethod.

	process suspendedContext method:
		ReflectivityExampleOnStack
		>> #exampleMethodWaitingOnSemaphoreInBlock.

	"here check that the method is on the stack"

	self
		assert: process suspendedContext method selector
		equals: #exampleMethodWaitingOnSemaphoreInBlock.
	"process should be running"
	self deny: process isTerminated.

	instance continue.
	0.1 seconds wait.
	self assert: process isTerminated.


	"It got executed!!"
	self assert: tag notNil
]

{ #category : 'tests' }
ReflectivityOnStackTest >> testExecuteWithWaitingWithLinkInstalledAfterExecutionOfMethodBeforeExecutionOfBlockAfterCreationOfBlockChangeMethodManually [

	| instance messageNode process |
	instance := ReflectivityExampleOnStack new.



	process := [
	           instance
		           exampleMethodWaitingOnSemaphoreWithBlockStoreInVariableBefore ]
		           fork.
	0.1 seconds wait.
	"If we install a link after we execute, we expect it to not matter for this test, as the new code will not be executed"
	messageNode := (ReflectivityExampleOnStack
	                >>
	                #exampleMethodWaitingOnSemaphoreWithBlockStoreInVariableBefore)
		               ast blockNodes first assignmentNodes second.
	link := MetaLink new
		        metaObject: self;
		        selector: #tagExec.
	messageNode link: link.
	self assert: messageNode hasMetalink.
	self
		assert: (ReflectivityExampleOnStack
			 >> #exampleMethodWaitingOnSemaphoreWithBlockStoreInVariableBefore)
				class
		equals: ReflectiveMethod.
	self assert: tag isNil.


	"Here we could do the on stack replacement manually"

	self
		assert: (ReflectivityExampleOnStack
			 >> #exampleMethodWaitingOnSemaphoreWithBlockStoreInVariableBefore)
				class
		equals: ReflectiveMethod.

	"make sure to create the compiledMethod"
	(ReflectivityExampleOnStack
	 >> #exampleMethodWaitingOnSemaphoreWithBlockStoreInVariableBefore)
		compileAndInstallCompiledMethod.

	process suspendedContext method: ReflectivityExampleOnStack
		>> #exampleMethodWaitingOnSemaphoreWithBlockStoreInVariableBefore.

	"here check that the method is on the stack"

	self
		assert: process suspendedContext method selector
		equals:
		#exampleMethodWaitingOnSemaphoreWithBlockStoreInVariableBefore.
	"process should be running"
	self deny: process isTerminated.

	instance continue.
	0.1 seconds wait.
	self assert: process isTerminated.


	"It got executed!!"
	self assert: tag notNil
]

{ #category : 'tests' }
ReflectivityOnStackTest >> testExecuteWithWaitingWithLinkInstalledAfterExecutionOfMethodBeforeExecutionOfBlockBeforeCreationOfBlockChangeMethodManually [

	| instance messageNode process |
	instance := ReflectivityExampleOnStack new.



	process := [ instance exampleMethodWaitingOnSemaphoreWithBlockAfter ]
		           fork.
	0.1 seconds wait.
	"If we install a link after we execute, we expect it to not matter for this test, as the new code will not be executed"
	messageNode := (ReflectivityExampleOnStack
	                >> #exampleMethodWaitingOnSemaphoreWithBlockAfter)
		               ast blockNodes first assignmentNodes second.
	link := MetaLink new
		        metaObject: self;
		        selector: #tagExec.
	messageNode link: link.
	self assert: messageNode hasMetalink.
	self
		assert: (ReflectivityExampleOnStack
			 >> #exampleMethodWaitingOnSemaphoreWithBlockAfter) class
		equals: ReflectiveMethod.
	self assert: tag isNil.


	"Here we could do the on stack replacement manually"

	self
		assert: (ReflectivityExampleOnStack
			 >> #exampleMethodWaitingOnSemaphoreWithBlockAfter) class
		equals: ReflectiveMethod.

	"make sure to create the compiledMethod"
	(ReflectivityExampleOnStack
	 >> #exampleMethodWaitingOnSemaphoreWithBlockAfter)
		compileAndInstallCompiledMethod.

	process suspendedContext method: ReflectivityExampleOnStack
		>> #exampleMethodWaitingOnSemaphoreWithBlockAfter.

	"here check that the method is on the stack"

	self
		assert: process suspendedContext method selector
		equals: #exampleMethodWaitingOnSemaphoreWithBlockAfter.
	"process should be running"
	self deny: process isTerminated.

	instance continue.
	0.1 seconds wait.
	self assert: process isTerminated.


	"It got executed!!"
	self assert: tag notNil
]

{ #category : 'tests' }
ReflectivityOnStackTest >> testExecuteWithWaitingWithLinkInstalledAfterExecutionOfMethodBeforeExecutionOfEmbeddedBlockAfterCreationChangeMethodManually [

	| instance messageNode process messageNode2 |
	instance := ReflectivityExampleOnStack new.

	process := [
	           instance
		           exampleMethodWaitingOnSemaphoreInOuterBlockWithEmbeddedBlockStoreInVariableBefore ]
		           fork.
	0.1 seconds wait.
	"If we install a link after we execute, we expect it to not matter for this test, as the new code will not be executed"
	messageNode := (ReflectivityExampleOnStack
	                >>
	                #exampleMethodWaitingOnSemaphoreInOuterBlockWithEmbeddedBlockStoreInVariableBefore)
		               ast blockNodes first assignmentNodes third.
	link := MetaLink new
		        metaObject: self;
		        selector: #tagExec.
	messageNode link: link.

	messageNode2 := (ReflectivityExampleOnStack
	                 >>
	                 #exampleMethodWaitingOnSemaphoreInOuterBlockWithEmbeddedBlockStoreInVariableBefore)
		                ast blockNodes second assignmentNodes second.
	link2 := MetaLink new
		         metaObject: self;
		         selector: #tag2Exec.
	messageNode2 link: link2.

	self assert: messageNode hasMetalink.
	self assert: messageNode2 hasMetalink.
	self
		assert: (ReflectivityExampleOnStack
			 >>
			 #exampleMethodWaitingOnSemaphoreInOuterBlockWithEmbeddedBlockStoreInVariableBefore)
				class
		equals: ReflectiveMethod.
	self assert: tag isNil.
	self assert: tag2 isNil.

	"make sure to create the compiledMethod"
	(ReflectivityExampleOnStack
	 >>
	 #exampleMethodWaitingOnSemaphoreInOuterBlockWithEmbeddedBlockStoreInVariableBefore)
		compileAndInstallCompiledMethod.

	process suspendedContext method: ReflectivityExampleOnStack
		>>
		#exampleMethodWaitingOnSemaphoreInOuterBlockWithEmbeddedBlockStoreInVariableBefore.

	"here check that the method is on the stack"

	self
		assert: process suspendedContext method selector
		equals:
		#exampleMethodWaitingOnSemaphoreInOuterBlockWithEmbeddedBlockStoreInVariableBefore.
	"process should be running"
	self deny: process isTerminated.

	instance continue.
	0.1 seconds wait.
	self assert: process isTerminated.

	"It got executed!!"
	self assert: tag notNil.
	self assert: tag2 notNil
]

{ #category : 'tests' }
ReflectivityOnStackTest >> testExecuteWithWaitingWithLinkInstalledAfterExecutionOfMethodInInlinedBlockChangeMethodManually [

	| instance messageNode process |
	instance := ReflectivityExampleOnStack new.



	process := [ instance exampleMethodWaitingOnSemaphoreInInlineBlock ]
		           fork.
	0.1 seconds wait.
	"If we install a link after we execute, we expect it to not matter for this test, as the new code will not be executed"
	messageNode := (ReflectivityExampleOnStack
	                >> #exampleMethodWaitingOnSemaphoreInInlineBlock)
		               ast blockNodes first sendNodes third.
	link := MetaLink new
		        metaObject: self;
		        selector: #tagExec.
	messageNode link: link.
	self assert: messageNode hasMetalink.
	self
		assert: (ReflectivityExampleOnStack
			 >> #exampleMethodWaitingOnSemaphoreInInlineBlock) class
		equals: ReflectiveMethod.
	self assert: tag isNil.


	"Here we could do the on stack replacement manually"

	self
		assert: (ReflectivityExampleOnStack
			 >> #exampleMethodWaitingOnSemaphoreInInlineBlock) class
		equals: ReflectiveMethod.

	"make sure to create the compiledMethod"
	(ReflectivityExampleOnStack
	 >> #exampleMethodWaitingOnSemaphoreInInlineBlock)
		compileAndInstallCompiledMethod.

	process suspendedContext method: ReflectivityExampleOnStack
		>> #exampleMethodWaitingOnSemaphoreInInlineBlock.

	"here check that the method is on the stack"

	self
		assert: process suspendedContext method selector
		equals: #exampleMethodWaitingOnSemaphoreInInlineBlock.
	"process should be running"
	self deny: process isTerminated.

	instance continue.
	0.1 seconds wait.
	self assert: process isTerminated.


	"It got executed!!"
	self assert: tag notNil
]

{ #category : 'tests' }
ReflectivityOnStackTest >> testExecuteWithWaitingWithLinkInstalledBeforeExecution [
	| instance messageNode process |
	instance := ReflectivityExampleOnStack new.
	
	"If we install a link before we execute, we just see its effects. This test is just to make sure we have the stuff working
	like we expect"
	messageNode := (ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphore) ast sendNodes first.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec.
	messageNode link: link.
	self assert: messageNode hasMetalink.
	self assert: (ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphore) class equals: ReflectiveMethod.
	self assert: tag isNil.
	
	process := [ instance exampleMethodWaitingOnSemaphore ] fork.

	"here check that the method is on the stack"	
	0.1 seconds wait.
	self assert: process suspendedContext sender method equals: ReflectivityExampleOnStack>>#exampleMethodWaitingOnSemaphore.
	"process should be running"
	self deny: process isTerminated.
	instance continue.
	0.1 seconds wait.
	self assert: process isTerminated.
	
	
	"here the tag should not be nil, the metalink should have been executed."
	self assert: tag notNil

]

{ #category : 'tests' }
ReflectivityOnStackTest >> testExecuteWithWaitingWithLinkInstalledInEmbeddedBlockAfterExecutionOfMethodAfterExecutionOfEmbeddedBlockChangeMethodManually [

	| instance process messageNode2 |
	instance := ReflectivityExampleOnStack new.

	process := [ instance exampleMethodWaitingOnSemaphoreInEmbeddedBlock ]
		           fork.
	0.1 seconds wait.
	"If we install a link after we execute, we expect it to not matter for this test, as the new code will not be executed"
	messageNode2 := (ReflectivityExampleOnStack
	                 >> #exampleMethodWaitingOnSemaphoreInEmbeddedBlock)
		                ast blockNodes second assignmentNodes second.
	link2 := MetaLink new
		         metaObject: self;
		         selector: #tag2Exec.
	messageNode2 link: link2.

	self assert: messageNode2 hasMetalink.
	self
		assert: (ReflectivityExampleOnStack
			 >> #exampleMethodWaitingOnSemaphoreInEmbeddedBlock) class
		equals: ReflectiveMethod.
	self assert: tag2 isNil.

	"make sure to create the compiledMethod"
	(ReflectivityExampleOnStack
	 >> #exampleMethodWaitingOnSemaphoreInEmbeddedBlock)
		compileAndInstallCompiledMethod.

	process suspendedContext method: ReflectivityExampleOnStack
		>> #exampleMethodWaitingOnSemaphoreInEmbeddedBlock.

	"here check that the method is on the stack"

	self
		assert: process suspendedContext method selector
		equals: #exampleMethodWaitingOnSemaphoreInEmbeddedBlock.
	"process should be running"
	self deny: process isTerminated.

	instance continue.
	0.1 seconds wait.
	self assert: process isTerminated.

	"It got executed!!"
	self assert: tag2 notNil
]

{ #category : 'tests' }
ReflectivityOnStackTest >> testExecuteWithWaitingWithLinkInstalledInOuterMethodAfterExecutionOfMethodAfterExecutionOfEmbeddedBlockChangeMethodManually [

	| instance messageNode process |
	instance := ReflectivityExampleOnStack new.

	process := [ instance exampleMethodWaitingOnSemaphoreInEmbeddedBlock ]
		           fork.
	0.1 seconds wait.
	"If we install a link after we execute, we expect it to not matter for this test, as the new code will not be executed"
	messageNode := (ReflectivityExampleOnStack
	                >> #exampleMethodWaitingOnSemaphoreInEmbeddedBlock)
		               ast sendNodes last parent.
	link := MetaLink new
		        metaObject: self;
		        selector: #tagExec.
	messageNode link: link.

	self assert: messageNode hasMetalink.
	self
		assert: (ReflectivityExampleOnStack
			 >> #exampleMethodWaitingOnSemaphoreInEmbeddedBlock) class
		equals: ReflectiveMethod.
	self assert: tag isNil.

	"make sure to create the compiledMethod"
	(ReflectivityExampleOnStack
	 >> #exampleMethodWaitingOnSemaphoreInEmbeddedBlock)
		compileAndInstallCompiledMethod.

	(process findContextsForMethod: process suspendedContext homeMethod)
		do: [ :ctx |
			ctx method: ReflectivityExampleOnStack
				>> #exampleMethodWaitingOnSemaphoreInEmbeddedBlock ].

	"here check that the method is on the stack"

	self
		assert: process suspendedContext sender sender method selector
		equals: #exampleMethodWaitingOnSemaphoreInEmbeddedBlock.
	"process should be running"
	self deny: process isTerminated.

	instance continue.
	0.1 seconds wait.
	self assert: process isTerminated.

	"It got executed!!"
	self assert: tag notNil
]

{ #category : 'tests' }
ReflectivityOnStackTest >> testExecuteWithWaitingWithLinksInstalledAfterExecutionOfMethodBeforeExecutionOfOuterAndEmbeddedBlockAfterCreationChangeMethodManually [

	| instance messageNode process messageNode2 |
	instance := ReflectivityExampleOnStack new.

	process := [
	           instance
		           exampleMethodWaitingOnSemaphoreInMethodWithOuterBlockStoreInVariableBefore ]
		           fork.
	0.1 seconds wait.
	"If we install a link after we execute, we expect it to not matter for this test, as the new code will not be executed"
	messageNode := (ReflectivityExampleOnStack
	                >>
	                #exampleMethodWaitingOnSemaphoreInMethodWithOuterBlockStoreInVariableBefore)
		               ast blockNodes first assignmentNodes second.
	link := MetaLink new
		        metaObject: self;
		        selector: #tagExec.
	messageNode link: link.

	messageNode2 := (ReflectivityExampleOnStack
	                 >>
	                 #exampleMethodWaitingOnSemaphoreInMethodWithOuterBlockStoreInVariableBefore)
		                ast blockNodes second assignmentNodes second.
	link2 := MetaLink new
		         metaObject: self;
		         selector: #tag2Exec.
	messageNode2 link: link2.

	self assert: messageNode hasMetalink.
	self assert: messageNode2 hasMetalink.
	self
		assert: (ReflectivityExampleOnStack
			 >>
			 #exampleMethodWaitingOnSemaphoreInMethodWithOuterBlockStoreInVariableBefore)
				class
		equals: ReflectiveMethod.
	self assert: tag isNil.
	self assert: tag2 isNil.

	"make sure to create the compiledMethod"
	(ReflectivityExampleOnStack
	 >>
	 #exampleMethodWaitingOnSemaphoreInMethodWithOuterBlockStoreInVariableBefore)
		compileAndInstallCompiledMethod.

	process suspendedContext method: ReflectivityExampleOnStack
		>>
		#exampleMethodWaitingOnSemaphoreInMethodWithOuterBlockStoreInVariableBefore.

	"here check that the method is on the stack"

	self
		assert: process suspendedContext method selector
		equals:
		#exampleMethodWaitingOnSemaphoreInMethodWithOuterBlockStoreInVariableBefore.
	"process should be running"
	self deny: process isTerminated.

	instance continue.
	0.1 seconds wait.
	self assert: process isTerminated.

	"It got executed!!"
	self assert: tag notNil.
	self assert: tag2 notNil
]

{ #category : 'tests' }
ReflectivityOnStackTest >> testExecuteWithWaitingWithTwoLinksInstalledAfterExecutionBeforeAndAfterInstructionChangeInterruptedMethodManually [

	| instance thirdMessageNode secondMessageNode process |
	instance := ReflectivityExampleOnStack new.

	process := [ instance exampleMethodWaitingOnSemaphoreDirectly ] fork.
	0.1 seconds wait.
	"If we install a link after we execute, we expect it to not matter for this test, as the new code will not be executed"
	thirdMessageNode := (ReflectivityExampleOnStack
	                     >> #exampleMethodWaitingOnSemaphoreDirectly) ast
		                    sendNodes third.

	link := MetaLink new
		        metaObject: self;
		        selector: #tagExec.

	thirdMessageNode link: link.

	self assert: thirdMessageNode hasMetalink.
	self
		assert: (ReflectivityExampleOnStack
			 >> #exampleMethodWaitingOnSemaphoreDirectly) class
		equals: ReflectiveMethod.
	self assert: tag isNil.
	self assert: tag2 isNil.

	"we install second link"
	secondMessageNode := (ReflectivityExampleOnStack
	                      >> #exampleMethodWaitingOnSemaphoreDirectly)
		                     ast sendNodes second.
	link2 := MetaLink new
		         metaObject: self;
		         selector: #tag2Exec.
	secondMessageNode link: link2.

	self assert: thirdMessageNode hasMetalink.
	self assert: secondMessageNode hasMetalink.
	self
		assert: (ReflectivityExampleOnStack
			 >> #exampleMethodWaitingOnSemaphoreDirectly) class
		equals: ReflectiveMethod.
	self assert: tag isNil.
	self assert: tag2 isNil.

	"make sure to create the compiledMethod"
	(ReflectivityExampleOnStack
	 >> #exampleMethodWaitingOnSemaphoreDirectly)
		compileAndInstallCompiledMethod.

	process suspendedContext method:
		ReflectivityExampleOnStack
		>> #exampleMethodWaitingOnSemaphoreDirectly.

	"here check that the method is on the stack"

	self
		assert: process suspendedContext method selector
		equals: #exampleMethodWaitingOnSemaphoreDirectly.
	"process should be running"
	self deny: process isTerminated.

	instance continue.
	0.1 seconds wait.
	self assert: process isTerminated.


	"only the metalink on the third message node got executed, because the second message node had already been executed"
	self assert: tag notNil.
	self assert: tag2 isNil
]

{ #category : 'tests' }
ReflectivityOnStackTest >> testExecuteWithWaitingWithTwoLinksInstalledAfterExecutionBeforeAndAfterInstructionChangeMethodManually [

	| instance thirdMessageNode secondMessageNode process |
	instance := ReflectivityExampleOnStack new.

	process := [ instance exampleMethodWaitingOnSemaphore ] fork.
	0.1 seconds wait.
	"If we install a link after we execute, we expect it to not matter for this test, as the new code will not be executed"
	thirdMessageNode := (ReflectivityExampleOnStack
	                     >> #exampleMethodWaitingOnSemaphore) ast
		                    sendNodes third.

	link := MetaLink new
		        metaObject: self;
		        selector: #tagExec.

	thirdMessageNode link: link.

	self assert: thirdMessageNode hasMetalink.
	self
		assert:
		(ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphore)
			class
		equals: ReflectiveMethod.
	self assert: tag isNil.
	self assert: tag2 isNil.

	"we install second link"
	secondMessageNode := (ReflectivityExampleOnStack
	                      >> #exampleMethodWaitingOnSemaphore) ast
		                     sendNodes second.
	link2 := MetaLink new
		         metaObject: self;
		         selector: #tag2Exec.
	secondMessageNode link: link2.

	self assert: thirdMessageNode hasMetalink.
	self assert: secondMessageNode hasMetalink.
	self
		assert:
		(ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphore)
			class
		equals: ReflectiveMethod.
	self assert: tag isNil.
	self assert: tag2 isNil.

	"make sure to create the compiledMethod"
	(ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphore)
		compileAndInstallCompiledMethod.

	process suspendedContext sender method:
		ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphore.

	"here check that the method is on the stack"

	self
		assert: process suspendedContext sender method selector
		equals: #exampleMethodWaitingOnSemaphore.
	"process should be running"
	self deny: process isTerminated.

	instance continue.
	0.1 seconds wait.
	self assert: process isTerminated.


	"only the metalink on the third message node got executed, because the second message node had already been executed"
	self assert: tag notNil.
	self assert: tag2 isNil
]

{ #category : 'tests' }
ReflectivityOnStackTest >> testExecuteWithWaitingWithTwoLinksInstalledAfterExecutionChangeInterruptedMethodManually [

	| instance messageNode variableNode process |
	instance := ReflectivityExampleOnStack new.

	process := [ instance exampleMethodWaitingOnSemaphoreDirectly ] fork.
	0.1 seconds wait.
	"If we install a link after we execute, we expect it to not matter for this test, as the new code will not be executed"
	messageNode := (ReflectivityExampleOnStack
	                >> #exampleMethodWaitingOnSemaphoreDirectly) ast sendNodes
		               third.

	link := MetaLink new
		        metaObject: self;
		        selector: #tagExec.

	messageNode link: link.

	self assert: messageNode hasMetalink.
	self
		assert:
		(ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphoreDirectly)
			class
		equals: ReflectiveMethod.
	self assert: tag isNil.
	self assert: tag2 isNil.

	"make sure to create the compiledMethod"
	(ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphoreDirectly)
		compileAndInstallCompiledMethod.

	process suspendedContext method:
		ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphoreDirectly.
		
	"we install second link"
	variableNode := messageNode receiver.
	link2 := MetaLink new
		         metaObject: self;
		         selector: #tag2Exec.
	variableNode link: link2.

	self assert: messageNode hasMetalink.
	self assert: variableNode hasMetalink.
	self
		assert:
		(ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphoreDirectly)
			class
		equals: ReflectiveMethod.
	self assert: tag isNil.
	self assert: tag2 isNil.
	
	"make sure to create the compiledMethod"
	(ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphoreDirectly)
		compileAndInstallCompiledMethod.

	process suspendedContext method:
		ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphoreDirectly.

	"here check that the method is on the stack"

	self
		assert: process suspendedContext method selector
		equals: #exampleMethodWaitingOnSemaphoreDirectly.
	"process should be running"
	self deny: process isTerminated.

	instance continue.
	0.1 seconds wait.
	self assert: process isTerminated.


	"Both metalinks got executed!!"
	self assert: tag notNil.
	self assert: tag2 notNil
]

{ #category : 'tests' }
ReflectivityOnStackTest >> testExecuteWithWaitingWithTwoLinksInstalledAfterExecutionChangeMethodManually [

	| instance messageNode variableNode process |
	instance := ReflectivityExampleOnStack new.

	process := [ instance exampleMethodWaitingOnSemaphore ] fork.
	0.1 seconds wait.
	"If we install a link after we execute, we expect it to not matter for this test, as the new code will not be executed"
	messageNode := (ReflectivityExampleOnStack
	                >> #exampleMethodWaitingOnSemaphore) ast sendNodes
		               third.

	link := MetaLink new
		        metaObject: self;
		        selector: #tagExec.

	messageNode link: link.

	self assert: messageNode hasMetalink.
	self
		assert:
		(ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphore)
			class
		equals: ReflectiveMethod.
	self assert: tag isNil.
	self assert: tag2 isNil.

	"make sure to create the compiledMethod"
	(ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphore)
		compileAndInstallCompiledMethod.

	process suspendedContext sender method:
		ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphore.
		
	"we install second link"
	variableNode := messageNode receiver.
	link2 := MetaLink new
		         metaObject: self;
		         selector: #tag2Exec.
	variableNode link: link2.

	self assert: messageNode hasMetalink.
	self assert: variableNode hasMetalink.
	self
		assert:
		(ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphore)
			class
		equals: ReflectiveMethod.
	self assert: tag isNil.
	self assert: tag2 isNil.
	
	"make sure to create the compiledMethod"
	(ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphore)
		compileAndInstallCompiledMethod.

	process suspendedContext sender method:
		ReflectivityExampleOnStack >> #exampleMethodWaitingOnSemaphore.

	"here check that the method is on the stack"

	self
		assert: process suspendedContext sender method selector
		equals: #exampleMethodWaitingOnSemaphore.
	"process should be running"
	self deny: process isTerminated.

	instance continue.
	0.1 seconds wait.
	self assert: process isTerminated.


	"Both metalinks got executed!!"
	self assert: tag notNil.
	self assert: tag2 notNil
]
