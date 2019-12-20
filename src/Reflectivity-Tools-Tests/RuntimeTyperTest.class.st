"
A small demo how to record type information at runtime with reflectivity.

We create this link:

	link := MetaLink new
		metaObject: #node;
		selector: #tagType:;
		arguments: #(value);
		control: #after.
		

this will add a message send to the AST node itself where you install it. It will call tagType: and as an argument hand over the result of the right hand side of the 
assignment. 

tagType: looks like this:

	tagType: anObject 
		variable propertyAt: #types ifAbsentPut: Set new.
		(variable propertyAt: #types) add: anObject class.

it uses the feature that we can annotate all the AST nodes easily with properties.


We can now use this on a simple example:

	examplePlus: arg
		| result |
		result := arg + 1.
		^result.


With this, we can run a test, see #testPlus.

What is not nice here yet is that, as we tag the AST variable node, we just tag the local node in
	result := arg + 1.

So in addition, one would need some AST visitor to gather all these infos (and maybe those of static assignments,
e.g. “a := 1” we know the type of a) and add these annotations to al the vars that are actually the same variable.
"
Class {
	#name : #RuntimeTyperTest,
	#superclass : #TestCase,
	#category : #'Reflectivity-Tools-Tests'
}

{ #category : #example }
RuntimeTyperTest >> examplePlus: arg [
	<sampleInstance>
	| result |
	result := arg + 1.
	^result.
]

{ #category : #tests }
RuntimeTyperTest >> testPlus [
	| link variable |
	link := MetaLink new
		metaObject: #node;
		selector: #tagType:;
		arguments: #(value);
		control: #after.

	(self class >> #examplePlus:) ast assignmentNodes do: [ :node | node link: link ].

	self assert: (self examplePlus: 1) equals: 2.
	self assert: (self examplePlus: 1.1) equals: 2.1.

	"link uninstall."

	variable := (self class >> #examplePlus:) ast assignmentNodes first variable.
	self assert: ((variable propertyAt: #types) includes: SmallInteger)
]
