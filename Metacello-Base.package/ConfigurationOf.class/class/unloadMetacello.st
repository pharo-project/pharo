unloading Metacello
unloadMetacello
	"Unload the classes that implement Metacello. Metacello is not needed once a project has been loaded, so it can safely be unloaded."

	"self unloadMetacello"

	<apiDocumentation>
	| gofer |
	gofer := (Smalltalk at: #Gofer) new.
	MCWorkingCopy allManagers do: [:wc |
		((wc packageName beginsWith: 'Metacello') or: [ wc packageName beginsWith: 'OB-Metacello' ])
			ifTrue: [ gofer package: wc packageName ]].
	gofer unload.