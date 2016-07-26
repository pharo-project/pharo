I am a widget used to expose basics about Spec and the use of  ButtonModel and LabelModel.
See ComposableModel

Try me with:
self example
self exampleDialog
self exampleNewLayout

I am composed of three ButtonModel and LabelModel.
The buttons represent moods, the label is use to simulate a screen.

When a button receive a click, its moods is displayed by the screen, that behaviour is implemented in initializePresenter.

self example open me simply in a default window using the default layout (defaultSpec).
self exampleDialogopen me simply in a dialog window that add a button 'Ok' and a button 'cancel', using the default layout.
self exampleNewLayout open me simply in a default window using the alternative layout that create column instead of rows.