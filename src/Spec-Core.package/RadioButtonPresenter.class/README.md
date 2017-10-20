A RadioButtonPresenter is a spec presenter for RadioButton.
See AbstractFormButtonPresenter

self example

! The method privateSetState: is really private and should only be used by RadioButtonGroup or RadioButtonGroupPresenter !

You can also have a look at RadioButtonGroupExample for a full example of how to use them with a group.

I provide more accessors on my super class ValueHolder
- actionWhenActivatedHolder
- actionWhenDeactivatedHolder
- enabledHolder
- stateHolder

I provide the variable canDeselectByClick and its accessors, it is a boolean representing if the radio can be deselect directly by the user using a click on it.

I specialize state: to use the variable canDeselectByClick.