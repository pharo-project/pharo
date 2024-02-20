# Debug Points

The debug point system is a model destined to replace the former breakpoint and watchpoint model.

## Types of debug points

**Debug points** are different types of **instrumentation that are used to debug**. 
For now, there are two concrete types of debug points: **breakpoints and watchpoints**.

As commonly known, breakpoints halts the program and watchpoints saves a value in its history when they are hit.
Basic debug points can also be used but they do nothing unless we add **behaviors** to them.

## Behaviors of debug points

A debug point can have **different types of behaviors** that will execute before the debug point is actually hit.

Among these behaviors, there are:

- *Once* behavior: the debug point becomes disabled after it is hit
- *Count* behavior: counts the number of times the debug point has been hit
- *Condition* behavior: sets a condition that must evaluate to `true` so that the debug point can actually be hit
- *Chain Link* behavior: combines debug points into a chain. In that case, when a debug point is hit in the chain, it disables itself and enables the next debug point in the chain. In other words, each debug points in the chain is hit only once in a specific order
- *Script* behavior: executes a script each time the debug point is hit
- *Transcript* behavior: logs a string to the Transcript each time the debug point is hit

All these behaviors can be set ![when creating a breakpoint from the Calypso browser in the method editor](https://github.com/adri09070/pharo/assets/97704417/c2f17276-2a3b-431c-bdec-0784bccaea2a).

To have more flexibility to configure existing debug points or to create different types of debug points, it is possible to use an API on `DebugPoint` objects or via the `DebugPointManager`.

It will be possible to configure debug points via a UI tool, the Debug Point Browser, which will be the object of another PR in NewTools.

## Debug Point Targets

Debug points can be installed on different types of targets:

- an *AST node*, ![which can be targeted by selecting source code in the Calypso browser](https://github.com/adri09070/pharo/assets/97704417/c2f17276-2a3b-431c-bdec-0784bccaea2a). In this case, the debug point is reached when the corresponding code is executed. 
  *NB: To put basic breakpoints quickly on AST nodes, ![it is still possible to double-click the left bar of in the method editor](https://github.com/adri09070/pharo/assets/97704417/99c5ad24-8224-4e37-9b4f-697a7092c786), just like with the ancient breakpoint system*
  **API:**
  + `DebugPointManager installNew: aDebugPointClass on: anASTNode`: instanciates a debug point class, configured with no behavior, and installs it on on an AST node 
  + `DebugPointManager installNew: aDebugPointClass on: aNode withBehaviors: aListOfBehaviorClasses`: instanciates a debug point class, configured with the list of behavior classes, and installs it on on an AST node 
- a *variable*, ![which can be targeted by selecting a variable in the variable view in the Calypso browser](https://github.com/adri09070/pharo/assets/97704417/d03143b9-dbdf-4b9b-b10e-a8b604488597). In this case, the debug point is reached each time the targeted variable is read or/and written, according to the debug point's configuration.
  **API:**
  + `DebugPointManager installNew: aDebugPointClass inClass: aClass onVariableAccessNamed: aSlotNameSymbol`: instanciates a debug point class, configured with no behavior, and installs it on the instance variable, whose name is given as argument, in a class hierarchy. The debug point is reached at each reading or writing of this variable.
  + `DebugPointManager installNew: aDebugPointClass inClass: aClass onVariableAccessNamed: aSlotNameSymbol withBehaviors: aListOfBehaviorClasses`: instanciates a debug point class, configured with the list of behavior classes, and installs it on the instance variable whose name is given as argument, in a class hierarchy. The debug point is reached at each reading or writing of this variable.
    + `DebugPointManager installNew: aDebugPointClass inClass: aClass onVariableReadNamed: aSlotNameSymbol`: instanciates a debug point class, configured with no behavior, and installs it on the instance variable whose name is given as argument, in a class hierarchy. The debug point is reached at each reading of this variable.
  + `DebugPointManager installNew: aDebugPointClass inClass: aClass onVariableReadNamed: aSlotNameSymbol withBehaviors: aListOfBehaviorClasses`: instanciates a debug point class, configured with the list of behavior classes, and installs it on the instance variable whose name is given as argument, in a class hierarchy. The debug point is reached at each reading of this variable.
  + `DebugPointManager installNew: aDebugPointClass inClass: aClass onVariableWriteNamed: aSlotNameSymbol`: instanciates a debug point class, configured with no behavior, and installs it on the instance variable whose name is given as argument, in a class hierarchy. The debug point is reached at each writing to this variable.
  + `DebugPointManager installNew: aDebugPointClass inClass: aClass onVariableWriteNamed: aSlotNameSymbol withBehaviors: aListOfBehaviorClasses`: instanciates a debug point class, configured with the list of behavior classes, and installs it on the instance variable whose name is given as argument, in a class hierarchy. The debug point is reached at each writing to this variable.
- an *object*, in order to install object-centric debug points. This type of target **wraps another target** (AST node or variable). In this case, the debug point is reached only if the wrapped target is reached and if the receiver is the target instance.
  **API:**
  + `DebugPointManager installNew: aDebugPointClass on: anASTNode forObject: anObject`: instanciates a debug point class, configured with no behavior, and installs it on on an AST node. The debug point is hit only if the receiver is the target object given as argument.
  + `DebugPointManager installNew: aDebugPointClass on: aNode withBehaviors: aListOfBehaviorClasses forObject: anObject`: instanciates a debug point class, configured with the list of behavior classes, and installs it on on an AST node. The debug point is hit only if the receiver is the target object given as argument.
  + `DebugPointManager installNew: aDebugPointClass forObject: anObject onVariableAccessNamed: aSlotNameSymbol`: instanciates a debug point class, configured with no behavior, and installs it on the instance variable, whose name is given as argument, only for the target object given as argument. The debug point is reached at each reading or writing of this variable.
  + `DebugPointManager installNew: aDebugPointClass forObject: anObject onVariableAccessNamed: aSlotNameSymbol withBehaviors: aListOfBehaviorClasses`: instanciates a debug point class, configured with the list of behavior classes, and installs it on the instance variable whose name is given as argument, only for the target object given as argument. The debug point is reached at each reading or writing of this variable.
    + `DebugPointManager installNew: aDebugPointClass forObject: anObject onVariableReadNamed: aSlotNameSymbol`: instanciates a debug point class, configured with no behavior, and installs it on the instance variable whose name is given as argument, only for the target object given as argument. The debug point is reached at each reading of this variable.
  + `DebugPointManager installNew: aDebugPointClass forObject: anObject onVariableReadNamed: aSlotNameSymbol withBehaviors: aListOfBehaviorClasses`: instanciates a debug point class, configured with the list of behavior classes, and installs it on the instance variable whose name is given as argument, only for the target object given as argument. The debug point is reached at each reading of this variable.
  + `DebugPointManager installNew: aDebugPointClass forObject: anObject onVariableWriteNamed: aSlotNameSymbol`: instanciates a debug point class, configured with no behavior, and installs it on the instance variable whose name is given as argument, only for the target object given as argument. The debug point is reached at each writing to this variable.
  + `DebugPointManager installNew: aDebugPointClass forObject: anObject onVariableWriteNamed: aSlotNameSymbol withBehaviors: aListOfBehaviorClasses`: instanciates a debug point class, configured with the list of behavior classes, and installs it on the instance variable whose name is given as argument, only for the target object given as argument. The debug point is reached at each writing to this variable.
