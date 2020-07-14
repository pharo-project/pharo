Class {
	#name : #PRPearMicroDownState,
	#superclass : #PRPearSyntaxState,
	#category : #'Pillar-Pear'
}

{ #category : #accessing }
PRPearMicroDownState >> exampleText [
	^ '# Header

A figure:
![Pharologo](https://files.pharo.org/media/logo/logo.png|size=80&caption=Our great figure.)

A link: [http://pharo.org](http://pharo.org)

Lists:
- item 1
  1. sub item 1
  3. sub item 2
- item 2

Horizontal line:
***

Common formats: **Bold**, _italic_, `monospace`

Pharo hyperlinks in code: 
- class `Point`, 
- method `Point class`, `Point>>#setX:setY:`, and 
- package `#’Microdown-Tests’ (for packages).

A codeblock:

```language=Pharo&caption=Beautiful&label=Fig1
   this is a codeblock
```

'
]

{ #category : #accessing }
PRPearMicroDownState >> extension [
	^ 'microdown'
]

{ #category : #accessing }
PRPearMicroDownState >> name [
	^ 'MicroDown'
]

{ #category : #accessing }
PRPearMicroDownState >> next [
	^ PRPearPillarState new
]

{ #category : #parsing }
PRPearMicroDownState >> parse: aText [
	^ (super parse: aText asString) asPillar
]

{ #category : #accessing }
PRPearMicroDownState >> parser [
	^ MicroDownParser 
]

{ #category : #accessing }
PRPearMicroDownState >> writer [
	^ PRMicrodownWriter
]
