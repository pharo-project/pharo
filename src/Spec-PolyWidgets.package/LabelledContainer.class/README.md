I am an elementary Spec widget that adds a LabelPresenter at another widget.

self example

I provide the method content: that take a class in parameter, it instantiates it in the variable subwidget and add it at the focusOrder. Note it does not reset the focusOrder.

I provide accessors for the label and the subwidget, but I specialize doesNotUnderstand: to send the message at the subwidget.

I provide four layouts on my class side, to chose the position of the label.