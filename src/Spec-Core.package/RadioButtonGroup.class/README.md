I am a group for radio buttons which ensures that only one button is activated at the same time.

I collaborate with
- RadioButtonPresenter are usually the buttons I manage.
- ValueHolder to link buttons.

I provide the following methods
- addRadioButton: to add the button in my collection and link it at the other.
- buttons getter of my collection.
- default: activate a button when it is built.

todo
- canDeselectByClick