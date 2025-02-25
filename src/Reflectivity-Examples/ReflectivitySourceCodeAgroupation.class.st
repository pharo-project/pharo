"
I'm an agroupation of ReflectivityCodeMorph
"
Class {
	#name : 'ReflectivitySourceCodeAgroupation',
	#superclass : 'Morph',
	#category : 'Reflectivity-Examples',
	#package : 'Reflectivity-Examples'
}

{ #category : 'accessing - defaults' }
ReflectivitySourceCodeAgroupation >> defaultColor [

	^ UITheme current baseColor
]

{ #category : 'initialization' }
ReflectivitySourceCodeAgroupation >> initialize [
	super initialize.
	self
 		layoutPolicy: TableLayout new;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		cellInset: 10
]

{ #category : 'instance creation' }
ReflectivitySourceCodeAgroupation >> newSourceCodeFor: aCompiledMethod [
	self addMorph: (ReflectivityCodeMorph compiledMethod: aCompiledMethod)
]
