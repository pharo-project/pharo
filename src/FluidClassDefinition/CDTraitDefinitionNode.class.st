"
I represent the definition of a Trait e.g.,

```
Trait named: TBehavior
	uses: Trait2;
	package: 'Kernel'
```
"
Class {
	#name : #CDTraitDefinitionNode,
	#superclass : #CDClassDefinitionNode,
	#category : #'FluidClassDefinition-New Nodes'
}

{ #category : #visitor }
CDTraitDefinitionNode >> acceptVisitor: aProgramNodeVisitor [
	
	self visitTraitDefinition: self
]
