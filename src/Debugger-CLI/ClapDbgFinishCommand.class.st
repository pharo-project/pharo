Class {
	#name : 'ClapDbgFinishCommand',
	#superclass : 'ClapDbgStepRelatedCommand',
	#category : 'Debugger-CLI',
	#package : 'Debugger-CLI'
}

{ #category : 'execution' }
ClapDbgFinishCommand >> initialize [

	super initialize.
	self commandName: #( 'finish' 'f' ).
	self commandHelp: 'executes the whole function'.
	self arguments: ''
]

{ #category : 'execution' }
ClapDbgFinishCommand >> performStepOn: aDebugger [
    | contextToLeave |
    contextToLeave := aDebugger context.
    
    "1. On avance tant qu'on est dans la méthode (l'équivalent de leur boucle)"
    [ aDebugger process suspendedContext == contextToLeave 
        or: [ aDebugger process suspendedContext hasSender: contextToLeave ] ] whileTrue: [ 
            aDebugger session stepInto: aDebugger process suspendedContext.
        ].
        
    "2. LE CHEF D'ŒUVRE : Comme la boucle s'arrête pile quand on quitte la méthode,"
    "on est maintenant au début de la ligne de l'appelant. Ton PC a avancé tout seul,"
    "et la vraie valeur calculée par la méthode a été transmise !"
]
