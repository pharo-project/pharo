"
I am an abtract superclass for compiler plugins.

The compiler consists of multiple passes:

```
source 
	- [ Scanner/Parser ]  -> 
AST 
	- [ SemCheck ] -> 
AST   <<HERE>>
	- [ ASTTranslator ] -> 
IR 
	- [ IRBuilder ] ->
 CompiledMethod
```

These plugins are called <<HERE>>, that is, after semantic analysis before generating the IR.
They are sorted by the message priority and handed the AST without making a copy (as plugins might just analyse the AST). If you modify the AST, you have to make a copy before!

## Note on plugin installation

Plugins can usually be used by classes by overriding the `compiler` method and adding the plugin there. But this can lead to problems if you use a plugin that is defined in the same package than the user during the loading of the package in Pharo.
This is due to the fact that the plugin can be used before its methods are installed. Simple plugins will not have this problem because I work as a no-op. But if a plugin overrides the method `transform:` and call other methods then it could happen. In that case, make sure to have the users of the pluggin in another package and to get your package dependencies right!


"
Class {
	#name : 'OCASTCompilerPlugin',
	#superclass : 'Object',
	#category : 'OpalCompiler-Core-Plugins',
	#package : 'OpalCompiler-Core',
	#tag : 'Plugins'
}

{ #category : 'accessing - defaults' }
OCASTCompilerPlugin class >> defaultPriority [
	"Use a high priority by default (a priority of 0 would be used by Reflectivity to be the last)"

	^ 100
]

{ #category : 'testing' }
OCASTCompilerPlugin class >> isAbstract [

	^ self == OCASTCompilerPlugin
]

{ #category : 'accessing - defaults' }
OCASTCompilerPlugin class >> priority [
	^ self defaultPriority
]

{ #category : 'accessing' }
OCASTCompilerPlugin >> priority [
	^ self class priority
]

{ #category : 'transforming' }
OCASTCompilerPlugin >> transform: ast [
	"No op"

	^ ast
]
