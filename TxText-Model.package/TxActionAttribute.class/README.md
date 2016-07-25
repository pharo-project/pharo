I have the following semantics:
- when a UI event attempts to interact with my text, I run it through my filter, and trigger the action if it passes.

Instance Variables:
#value - an action block, with an optional argument to receive the event