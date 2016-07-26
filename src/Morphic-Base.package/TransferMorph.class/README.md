This is a Morph that is used to visually indicate the progress of a drag operation, and also as a container for various bits of drag state information.

It polls the shift state in its step method to update its copy state (shift pressed = should copy).

And if you hit the Escape key while dragging, it aborts the drag operation.