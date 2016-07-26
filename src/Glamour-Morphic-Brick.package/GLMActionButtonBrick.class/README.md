A GLMActionButtonBrick is a basic iconified action button without label that supports mouse click, hover and unhover actions.

action: with object argument to set on mouse clicked action.
hoverAction action: with object argument to set on mouse hover action.
unhoverAction: with object argument to set on mouse unhover action.

User can set normal icon and icon that will be used when action button is hovered with mouse sending:
icon: aForm
hoverIcon: aForm

Instance Variables
	clickAction:		<Object>
	hoverAction:		<Object>
	unhoverAction:		<Object>

clickAction
	- action on mouse clicked

hoverAction
	- action on mouse hovered

unhoverAction
	- action on mouse unhovered
