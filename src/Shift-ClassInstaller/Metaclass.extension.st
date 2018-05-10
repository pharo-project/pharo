Extension { #name : #Metaclass }

{ #category : #'*Shift-ClassInstaller' }
Metaclass >> instanceVariableNames: instVarString [ 
	
	^ self slots: instVarString asSlotCollection 
]
