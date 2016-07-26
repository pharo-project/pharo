An GLMAccumulator is an implicit type of browser that has the following behavior:
- each input entity has associated a pane without any relationship with the other panes
- based on the input entity if there already exists a pane associated, it is selected via GLMPaneSelected
- if there isnt a pane, a new pane is created
- based on entityToSelect, the associated pane is searched and potentially selected
- when a pane is selected in the user interface, the activeEntity is populated with the entity behind the selected pane

Input ports:
- entity
- entityToSelect

Output ports:
- activeEntity