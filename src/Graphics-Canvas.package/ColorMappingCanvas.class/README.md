I'm an abstract class which introduce a filter between a drawing request and the final output, handled by #mapColor:

For each potential pixel operation like: source -> op -> output
it introducing a color mapping stage: source -> op -> mapping -> output

Then #mapColor: can be redefined in subclasses to implement more specific behavior. For example:
- ShadowDrawingCanvas will replace the rendered color with the color of shadow (if not transparent).
- AlphaBlendingCanvas will add or intensify alpha of rendered color.

For an easy to understand example see  #drawPolygon:color:borderWidth:borderColor: