cleanup
cleanUp: aggressive except: exclusions confirming: aBool
	"Clean up. When aggressive is true, this will destroy resources, change sets, etc.
	Leave out any classes specifically listed in exclusions."

	"Smalltalk cleanUp: true except: {Point . ChangeSet} 
		- will ask for confirmation"
	"SmalltalkImage current cleanUp: true except: {} confirming: false 
		- will not ask for confirmation"

	| classes |
	aBool ifTrue:[
		"Give the user a chance to bail"
		(self confirm: 'Cleanup will destroy change sets and more.
Are you sure you want to proceed?') ifFalse: [^self]].

	"Find all classes implementing #cleanUp or cleanUp:"
	classes := Smalltalk allClasses select: [:aClass| 
		(aClass class includesSelector: #cleanUp) 
			or: [aClass class includesSelector: #cleanUp:]].

	"Leave out the classes in the exclusion set"
	classes := classes reject: [:aClass| exclusions includes: aClass].

	"Arrange classes in superclass order, superclasses before subclasses.
	This will ensure that specific cleanup (like MethodDictionary compaction)
	will run after generic superclass cleanup (HashedCollection rehashing).
	Otherwise generic superclass cleanup might undo specific one (in this
	case rehashing will undo a good bit of MD compaction)."
	classes := Class superclassOrder: classes.

	"Run the cleanup code"
	classes 
		do:[:aClass| aClass cleanUp: aggressive]
		displayingProgress: [:aClass| 'Cleaning up in ', aClass name].