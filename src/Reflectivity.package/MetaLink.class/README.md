Metalinks are used to annotate other AST nodes. An annotated AST is expanded, compiled and executed on the fly thanks to the ReflectiveMethod/CompiledMethod Twin. 

For a given node metalinks can be put at different positions:

- before: The metalink is executed before the execution of the node.  
- instead: The metalink is executed insted the node.
- after: The metalink is executed after the execution of the node.
(... later: onError,  Do we have an #around instead of #instead?)

Not all the nodes provide all the position. For example, literals don't provide onError and onSuccess positions.

metaObject: The target object to call
selector: send this selector
arguments

condition:  turn link on/off 
level: Meta Level at which the link is active
------ Examples -----

MetaLink new 
	metaObject: Halt;
	selector: #now.
	
MetaObject new 
	metaObject: [ self halt ];
	selector: #value.