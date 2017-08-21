I am a Spec widget  allowing a user to choose a Color by its RGB values and providing a preview.

self example

I am composed of a RGBSliders and an ImageModel that display simply a form of the current color.

I provide the following methods
- color a shortcut color to access the current color of the RGBSliders.
- makeNewForm return a fresh copy of the current displayed form.

I provide extentForPreview, a point used to define the size of the preview.