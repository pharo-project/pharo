# How to migrate an inspection

You may have noticed than in Pharo 13 we removed one of the ways we use to create `inspections` (you know, those add-ons to a class that allows you to enhance the inspector showing the object in the way you prefer).  
I say "one of the ways" because in fact we had two ways before, but we were often using the one we removed, and that can be an annoyance for people that knew just that way.  

## What changed?

Before, to declare an inspection you were probably using the `single selector` way, something like this:

``` smalltalk
Collection >> inspectionItems [
    <inspectorPresentationOrder: 0 title: 'Items'> 
    
    ^ SpTablePresenter new
        addColumn: (SpIndexTableColumn new 
            title: 'Index';
            sortFunction: #yourself ascending;
            beNotExpandable;
            yourself);
        addColumn: (SpStringTableColumn new  
            title: 'Value'; 
            evaluated: [ :each | StObjectPrinter asTruncatedTextFrom: each ];
            beSortable;
            yourself);
        items: self asOrderedCollection;
        yourself
]
```

And this is not usable anymore.
Before explaining how you can migrate, you will wonder...

## Why we changed it?

This is very simple. To build a presenter is in part to construct something like a DOM (a Document Object Model, like the one you build with HTML in your browser).
A presenter is built in the context of an `application` and must have an `owner` to be part of a DOM, the only exception is root components (think on main windows), that can not have an `owner`, but instead they must belong to an `application`.
**Inspections** are not other than a view inside an `inspector` presenter, and that inspector belongs to the Pharo application, therefore we need to build it inside its rightful owner, the inspector (in fact is a part of an inspector, but this is just an implementation detail).

Now, that you understand **why** we can explain...

## How why changed it?

In **Spec**, we have several ways to easy the building of a presenter inside a DOM, but we favor the use of the *scripting language* provided in each presenter (you know, the one you use when you implement `initializePresenters`). In this implementation, you do not instantiate directly a presenter class (in the previous example, `SpTablePresenter`) to have what you need to build your view, but instead you cal the *factory method* provided (e.g. `newTable`).
But since the the inspections are part of any random object, not a presenter, we need to provide this scripting interface as a builder to the inspector methods.
This will be the only argument the inspection method will receive.
If we rewrite the previos method, it will end looking like this:

``` smalltalk
Collection >> inspectionItems: aBuilder [
    <inspectorPresentationOrder: 0 title: 'Items'> 
    
    ^ aBuilder newTable
        addColumn: (SpIndexTableColumn new 
            title: 'Index';
            sortFunction: #yourself ascending;
            beNotExpandable;
            yourself);
        addColumn: (SpStringTableColumn new  
            title: 'Value'; 
            evaluated: [ :each | StObjectPrinter asTruncatedTextFrom: each ];
            beSortable;
            yourself);
        items: self asOrderedCollection;
        yourself
]
```

This is trivial :)

There is however a special case...

### What happen if I have a custom presenter?

Some times we have not just a table or a list to present but a whole custom inspector. This is the case for example of the "Meta" inspection, that will show a complete mini-browser as last option of the inspector.
Well, in this case we need also to create it inside the DOM, using the `instantiate:` (or `instantiate:on:` if we need to provide a model) message:

``` smalltalk
Object >> inspectionMeta: aBuilder [
    <inspectorPresentationOrder: 999 title: 'Meta'>

    ^ aBuilder instantiate: StMetaBrowserPresenter on: self
]
```

In summary, to migrate your inspections to the new format (which in fact was there before, just not used a lot), is a task that can be completed in seconds :)
