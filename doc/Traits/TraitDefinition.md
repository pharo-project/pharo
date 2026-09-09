#Traits in Pharo

## Trait in a Nutshell

Traits are kind of class fragments: they define methods and state. 
Classes or traits can use traits. When they do, they acquire all the trait methods and behavior. 
Now the composer (a class or a trait) can define to change some part of the trait it uses. 
With trait, definitions in the composer always take precedence over the definition defined in the used traits. 

### Simple Example

#### Definition

Here we define a trait named `TFun`. It defines a simple method named what which returns a string. 

```Smalltalk
Trait << #TFun
	package: 'MyPackage'

TFun >> what
	^ 'It is fun'
```

#### Use

Now we define a class named `WithFun` that uses the traits `TFun` (the message `traits:` is the message to add traits to a class).

```
Object << #WithFun
	traits: { TFun };
	package: 'MyPackage'
```

Then we instantiate the class and send the message `what`. The message is resolved and the expected result is return. 
The class WithFun  has all the methods defined in the trait `TFun`.

```
WithFun new what
> 'It is fun'
```

### Method Definition

You can then add a new method to the trait. As any other methods it can send message to existing methods.
Here we add the method `superWhat`.

```
TFun >> superWhat

	^ self what , '. Really fun'
```

We can now send this message to instances of the class `WithFun`.

```
WithFun new superWhat
> 'It is fun. Really fun'
```

### Other users

The same trait can be used by multiple classes or traits. 
Here we defined a new class named `WithFun2` that uses the trait `TFun`.

```
Object << #WithFun2
	traits: { TFun };
	package: 'MyPackage'
```

And we can see that instances of this new class can execute the methods
of the trait.

```
WithFun2 new superWhat
> 'It is fun. Really fun'
```

### Overriding trait methods 

Now image that the class `WithFun` needs to have another behavior. 
We can redefine locally a method. This is this locally define method that will be executed
instead of the method from the trait. This only impacts the class defining the specific method. 

The class `WithFun redefines the method `superWhat` as follows: 
```
WithFun >> superWhat

	^ self what , '. Really fun and powerful'
```

Now the following expression shows that only the implementation of the class `WithFun` gets modified
by the new definition.

```
{ WithFun new . WithFun2 new} collect: #superWhat 
>> #('It is fun. Really fun and powerful' 'It is fun. Really fun')
```




### Using multiple traits



You can also use multiple Traits with your class with the `#+` message.

```Smalltalk
MySuperClass << #MyClass
	traits: { TNameOfMyTrait + TNameOfMySecondTrait };
	package: 'MyPackage'
```

## Abstract methods

We might need to call a method for which the implementation will be specific to the class using the trait. To manage this case, a Trait can hold methods that explicitely declare that user should define it. These methods contain a call to `#explicitRequirement` message.

```Smalltalk
TMyTrait >> addButton: aButton
	self buttons add: aButton
```

```Smalltalk
TMyTrait >> buttons
	^ self explicitRequirement
```

> Some Pharo developers create Traits with all their methods calling `#explicitRequirement` message. Doing this kind of simulate an interface (as Java's interfaces). Users of one of these traits thus declare that they support the interface it defines and override all methods defined by the trait.

## Stateful traits

Since Pharo 7, it is possible to add slots to Traits. This will make you trait a stateful trait. 

Examples:

```Smalltalk
Trait << #MDLWithConfigurableRightPanel
	slots: { #panelComponent . #toolbar };
	package: 'MaterialDesignLite-Extensions'
```

```Smalltalk
Trait << #FamixTWithEnumValues
	slots: { #enumValues => FMMany type: #FamixTEnumValue opposite: #parentEnum };
	package: 'Famix-Traits-EnumValue'
```

## Traits initialization

Traits do not include a way to initialize classes using them, it relies on conventions.

One way to manage this might be to implement a method named `initializeTMyTraitName` on each traits needing an initialization and to call all those methods on the class using them.

In case of trait composition (See [Trait composition](#trait-composition)), a trait composed of other traits can also implement a initialize method calling the one of the Traits it includes.

## Customize method received from a Trait
When a class uses a trait, it is possible for it to reject or alias some methods.

### Reject some methods received from the trait
In some case it is needed to reject a method of a Trait. It can be achieved using `#-` message.

```Smalltalk
TestCase << #StackTest
	traits:  { TEmptyTest - {#testIfNotEmptyifEmpty. #testIfEmpty. #testNotEmpty} + (TCloneTest - {#testCopyNonEmpty}) };
	slots: { #empty. #nonEmpty } ;
	package: 'Collections-Tests-Stack'
```

### Alias some methods received from the trait
It is possible to alias some methods received from a trait. If, for example you alias `#aliasedMethod` with `#methodAlias` as shown below, your class will hold both `#methodAlias` and `#aliasedMethod`.

```
Object << #MyObjectUsingTraitByAliasingMethod
	traits:  { TTraitToBeUsed @ { #methodAlias -> #aliasedMethod } }; 
	package: 'TestTraitAliasing'
```

Here is a simple example. Consider a situation when a trait `TLocated` implements a method `moveTo:` that defines the movement of an object to a given cell. The user of this trait needs to implement the post-movement operation. Usually, this would be done by overriding the `moveTo:` method and calling `super moveTo: aCell` in the first line of the new implementation. However, the super calls can not be used with traits as they install methods directly into the code of their users. The simple workaround would be to create an allias `basicMoveTo:` for the trait method and then call it from the new `moveTo:` method implemented by the user class:

```st
TLocated >> moveTo: aCell
    "Define the movement"

Object << #Antelope
    traits:  { TLocated @ { #basicMoveTo: -> #moveTo: } };
    ...

Antelope >> moveTo: aCell
    self basicMoveTo: aCell.
    "Do some post-movement actions"
```

## Customize instance variables received from a (stateful) Trait
When a class uses a trait, it is possible for it to reject or alias some instance variables.

### Reject some instance variables received from the trait
In some case it is needed to reject an instance variable of a Trait. It can be achieved using `#--` message. It works similarly to methods rejecting explaining in previous section.

```
Object << #MyObjectUsingTraitByRejectingInstVar
	traits: {} TTraitToBeUsed asTraitComposition -- #instVarNameToRemove };
	package: 'TestTraitAliasing'
```

> `#asTraitComposition` needs to sent to the trait because `#--` message is not understood by trait but by trait composition.

### Alias some instance variables received from the trait
It is possible to alias some instance variables received from a trait. If, for example you alias `#aliasedInstVar` with `#instVarAlias` as shown below, your class will hold both `#instVarAlias` and `#aliasedInstVar`.

```
Object << #MyObjectUsingTraitByAliasingInstVar
	traits: { (TTraitToBeUsed >> { #instVarAlias -> #aliasedInstVar }) } ;
	package: 'TestTraitAliasing'
```

## Trait composition

Traits are composable, this mean that you can have Traits using other traits. It is done in the same way than class using a Trait:

```Smalltalk
Trait << TMyComposedTrait
	traits:  { TMyFirstTrait + TMySecondTrait} ;
	package: 'MyPackage'
```

Example:

```Smalltalk
Trait << #EpTEventVisitor
	traits:  { EpTCodeChangeVisitor };
	package: 'Epicea-Visitors'
```
## Conflicts

Two kinds of *conflicts* can happen with methods implemented on Traits.

1. A method is present on a used Trait, but the class using this Trait also implements this method. In that case, the method lookup will select the method from the class. It is an equivalent of an override of method.

2. Two traits implementing the same method are used. In that case, if the method is called it will raise an error `traitConflict`.

A way to solve both cases is to use method aliasing and to remove the conflicting method:
```Smalltalk
Object << #MyObjectUsingTraitByAliasingMethod
	traits: { TTraitToBeUsed @ { #methodAlias -> #conflictingMethod } - { #conflictingMethod } };
	package: 'TestTraitAliasing'
```

Another way to solve case 2. is to implement a method on the class using the trait in order to chose the behavior wanted.
