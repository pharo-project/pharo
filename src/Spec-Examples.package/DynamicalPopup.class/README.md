I am a widget used to expose basics about Spec and the use of TextInputFieldModel.
See DynamicComposableModel

Try me with
self exampleAlternativeToolbar
self exampleDefaultToolbar

The construction of dynamic widgets
===============================

I am composed of TextInputFieldModel and LabelModel, but notice there is neither variables, nor accessors.

dynamicLayout set a layout in a variable of my superclass (DynamicComposableModel).

The toolbar of the dialog window 
============================

self exampleDefaultToolbar open me in a default dialog window (add an 'ok' button and a 'cancel' button).
Then it set a behaviour at the 'ok' button (implemented in the method defaultToolbarConfiguration).

self exampleAlternativeToolbar open me in a default dialog window (add an 'ok' button and a 'cancel' button).
Then calls alternativeToolbar that:
- create an OkToolbar (it only have an 'ok' button).
- set a behaviour at the 'ok' button of the OkToolbar.
- replace the toolbar of the dialog window by the created OkToolbar.