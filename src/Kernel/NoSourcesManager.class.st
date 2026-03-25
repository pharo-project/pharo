"
I am a null source code manager. I will not register anything and will not retrieve anything. 

You can use me to deploy a Pharo application without shipping a .changes and .sources file as long as the application does not access source code. In order to do that you can execute:

```st
	NoSourcesManager beCurrent
```
"
Class {
	#name : 'NoSourcesManager',
	#superclass : 'SourcesManager',
	#category : 'Kernel-Sources',
	#package : 'Kernel',
	#tag : 'Sources'
}

{ #category : 'accessing' }
NoSourcesManager >> commentTimeStampAt: sourcePointer [

	^ ''
]

{ #category : 'copying' }
NoSourcesManager >> imageMovedWithNewName: newNameWithoutSuffix [
	"Nothing to do here"

	
]

{ #category : 'accessing' }
NoSourcesManager >> protocolAt: sourcePointer [

	^ 'unfound protocol'
]

{ #category : 'querying' }
NoSourcesManager >> reportString [

	^ 'No source manager.'
]

{ #category : 'accessing' }
NoSourcesManager >> sourceCodeAt: sourcePointer [

	^ nil
]

{ #category : 'accessing' }
NoSourcesManager >> timeStampAt: sourcePointer [

	^ ''
]

{ #category : 'logging' }
NoSourcesManager >> writeClassCreation: aClass [

]

{ #category : 'logging' }
NoSourcesManager >> writeClassRemoved: aClass [

]

{ #category : 'logging' }
NoSourcesManager >> writeClassRenamed: aClass oldName: aString [

]

{ #category : 'writing' }
NoSourcesManager >> writeComment: aString stamp: aStamp of: aClass [

	
]

{ #category : 'logging' }
NoSourcesManager >> writeMethodRemoved: aMethod from: aBehavior [

]

{ #category : 'writing' }
NoSourcesManager >> writeSource: source of: aMethod withFooter: footer protocol: protocol timestamp: aTimestamp priorSourcePointer: priorSourcePointer [
	"Since we can not store the source pointer, we put back the property"

	aMethod propertyAt: #source put: source
]
