A GLMPresentation is the abstract class for the hierarchy of presentations. A presentation specifies how the pane (held in the pane instance variable) is going to be displayed. It typically reads at least the #entity port of a pane and populates at least the #selection port.

updateActions holds a collection of GLMUpdateAction that are used to update the presentation via announcements.

rawSelectionTransmissions holds a collection of transmission whose origins are this presentation's #rawSelection port. Destinations of this transmissions are on the pane. This collection always contains at least one transmission to the pane's #selection port. To transform the values travelling through this transmission use #send:. To add new transmissions, use #send:as:.

Because Glamour has a prototype-based design it relies on copying the presentations before installing them in panes (via transmissions). The parentPrototype instance variable keeps track of the presentation from which the current one was copied.

It raises:
- GLMContextChanged to let the world know that something has changed in the containing pane. This is typically used by the renderer to update the rendering.
- GLMPresentationUpdated to let the world know that the presentations wants to be updated because of reasons other than the pane context changed.