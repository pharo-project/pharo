GTInspectorMethodListFilter implements a filtering strategy for presentations based on method signatures (method selector and class).

The filter returns true for a method if the signature of that method is selected in the filter. To be taken into account during filtering a signature must be first added to the filter and the selected. Only signatures that have been first added can be selected. 