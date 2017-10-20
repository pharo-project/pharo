ComposablePresenter is an abstract class which represent a applicative presenter made to be composed with other ComposablePresenter

Most often, I am used to display other subclasses of ComposablePresenter or subclasses of AbstractWidgetPresenter.

I collaborate with 
-  ValueHolder to managed some of my variables.
- SpecFocusOrder when no focusOrder are specified.
- SpecLayout
- WindowPresenter or DialogWindowPresenter to display myself.

A new subclass of ComposablePresenter must at least define initializeWidgets and defaultSpec on the class side.

I provide the following methods
- aboutText: set the text displayed in the About.
- title: set the title of the window.
- extent: set the initial size of the window, alternative possibility is define again initialExtent, it must be called before have displayed the widget. 
- focusOrder: set an instance what manage the order of focus of the keyboard navigation.
- bindKeyCombination:toAction: create a shortcut on keyboard what perform the block of the  action.
- owner getter of my parent.
- window getter of the window displaying me.

On my class side, I provide methods returning standard height and width for some elements.

todo
- announce:
- applyMenuModel: and neglectMenuModel:
- keyStrokeForNextFocus: , giveFocusToNextFrom: and takeKeyboardFocus
- ensureExtentFor:
- hide
- needRebuild
- on:do:
- update:
- widget

Window
=======

- openWithSpec instantiate a WindowPresenter using the lookup to found the layout to use.
- openDialogWithSpec instanciate a DialogWindowPresenter using the lookup to found the layout to use.
Their variants openWithSpec: and openDialogWithSpec:  use the layout parameter.

These methods can be useful to manage the window
- isDisplayed return true if the window of the widget is displayed
- hasWindow return true if the widget have a window
- centered to center the window in the world.
- delete to delete the window.

todo
- cancelled
- setModal:
- windowIcon:


Instantiation
===========

* initializeWidgets is called by the initialize method, it should contain initialization of subwidgets and of the focusorder.

The instantiation of a subwidget should use one of that way
- instantiate: take a class in parameter and return the created instance.
- methods named 'new' followed by a widget name are shortcut working with instatiate:

Usually, the subwidgets must be added in the focusOrder using something like 'self focusOrder add: accessor of  the  subwidget'

Note that instantiateModels: is legacy code in ComposablePresenter and must not be used. It will be deprecated and removed.

* initializePresenter is called by the initialize method after initializeWidgets, it should manage interaction of subwidgets.

Methods named 'when' followed by an event provide hook to perform the action in parameter.

Note
-------
Be careful about code order if you are overriding initialize method.
Normally in Spec initializing instance variables should be done BEFORE calling super initialize (so the opposite of the normal approach), because super initialize calls initalizeWidgets and initializePresenter that normally would make use of those variables.

Layout
======

See SpecLayout

defaultSpec or a method containing the pragma <spec: #default> must be defined in the class side of my subclasses.
It contains informations about how place its elements.
It possible to define more than one method to give the possibility to use another layout, by default the one containing the pragma will be used if it exists, if not defaultSpec will be used.