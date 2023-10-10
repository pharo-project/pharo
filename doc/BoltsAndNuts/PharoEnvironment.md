# About the Pharo Environment

This is little introduction for the Pharo environment and tools.

## Keyboard Shortcuts

Pharo has a variety of useful keyboard shortcuts that help you navigate its environment without always using a mouse.

### Browser Shortcuts

There are many keyboard shortcuts available in Pharo:

- You can see them at the end of the System Browser's context menu lines by right-clicking on any area (packages, classes, protocols, methods or code editor).
- Shortcuts are typically multi-key combinations, where you hold a modifier key and press the listed secondary key. For example if something is listed as CMD-F, CMD-C you would press and hold the CMD key while typing  F and then C. 
There are also two-letter shortcuts, such as (CMD-GA) or (CMD-AA). You must press and hold the CMD key while typing G and release, type A and release.

- The modifier key varies between platforms:
  - Windows = CTRL
  - Mac/OSX = CMD
  - Linux = CTRL

### Spotter/Global search

Similar to the Spotlight search on OSX, or the Windows Search charm - Pharo has a global code search tool that is activated using:  Shift+Enter

This search tool shows a popup window in the center of the screen where you can incrementally search for classes, packages, symbols,  pragmas, files, folders and many other types of data.  As you type a search query  all available searches are performed at the same time and results are displayed as they are discovered and grouped based on the type of the search.

You can cursor/keyboard navigate through the results, or press enter to spawn a new tool to examine the selected result.
The following shortcuts are available:

- Ctrl+Arrow_right dives in and restricts the available types of searches only to the selected element.
- Ctrl+Shift+Arrow_right dives in and shows all the search results from a category.
- Ctrl+Arrow_left dives out to the previous search
- Ctrl+p toggles the preview pane
- Enter spawns a new tool to examine the selected result
- Ctrl+Shift+Arrow_down/Ctrl+Shift+Arrow_down navigate through different types of searches
- Shift highlights all buttons from the UI (only works with newer VMs)

### Visual Exploration

A useful way of discovering how things work in Pharo, is to point your mouse cursor at something on the screen (e.g. a browser button) and invoke a visual Halo selection by pressing: 

Alt+Shift + left mouse click

If you continue clicking the left mouse button (while holding Alt+Shift), the halo will expand it's selection to the current item's parent. This is useful for traversing the lineage of graphical Morph's to see what contains what.

The halo has a series of icons, which if you hover over them will give you some balloon help about their operation. For exploring, the spanner icon is useful for letting you inspect or browse the current item to locate where the actual code is implemented.

To remove the halo, simply click anywhere outside of the current halo.

It is possible to directly explore a visual object and bypass using the halo mechanism by pressing:

Ctrl+Shift + left mouse click

This will give you a menu for the current item with options to inspect, debug etc.

## Browsing and Navigating Code

Finding and navigating through code in Pharo is an important part of developing programs in Smalltalk.

### System Browser (Calypso)

The System Browser (available in the desktop World menu), is the primary way of viewing your source code. This browser framework has been rewritten in Pharo and is often referred to as Calypso (see the other top level help item for more technical details).

The previous browser was called Nautilus, maybe you can see some outdated reference to Nautilus in some places.

#### Showing Hierarchies

Calypso defaults to a simple 'Flat' display of the list of classes that are in the currently selected package (far left). If you want to see the hierarchy of a particular class, first select it and then press (toggle) the 'Flat' button, to show show its 'Hierarchy'. Pressing this button again will toggle back to the 'Flat' model.

#### Browser History

The browser records the history of methods and classes you have visited similar to a Web Browser. The dropdown list in the middle right of the panes (above the lower text pane) shows this history, and clicking on one of its items will navigate to that item. This is convenient if you have clicked on a different package, or navigated to superclass and want to return back to where you were. 

In the same area, there is the versions button. You can easily revert any code changes to any time in the past.

#### Customizing

The system browser can also be configured with a different title as well as optional plugins. These are all available in the window dropdown menu in the top right of the title bar (normally a small triangle).

### Spotter

Use the Shift+Enter global keystroke to activate a popup window that lets you incrementally search for methods, classes, packages, pragmas, files, folders and many other types of data.

### Finder

The code Finder browser is available from the World | Tools menu. The Finder lets you enter some general text in top edit field and then categorize it as either text, or language construct in the dropdown list to refine your search. There are more detailed instructions in the bottom pane of the browser detailing the different options.

### Workspace

In any workspace you can type or select any text and then perform an 'code search' (context menu item) that will look for matching senders, implementors, references or method source.

## Icons

There are many icons that are visible in the environment, each with different meanings.

### General

An orange smudge in the top right of any editable text field indicates that the field has been modified and should be saved with CMD+S.

### Class Pane

In the System Browser class pane, you will see the following icons appear next to specific types of class objects:

- Red exclamation mark: a missing class comment. The Pharo team recommend that all classes should describe their intent with a comment. 
- Gray dot: a TestCase
- Yellow Lightening bolt: an Exception
- 3 Coloured Balls: a Collection
- Blue Speech bubble: an Announcement
- Blue Paragraph symbol: a String
- Gray Epsilon symbol: a Magniture
- Blue Matrix box: a graphcial Morph
- Purple Ball with T: a Trait

### Protocol Pane

In the System Browser protocol pane, you will see the following icons appear next to specific types of protocol objects:

- Yellow diamond: initialization methods
- Red Square: private methods

### Method Pane

In the System Browser methods pane, you will see the following icons appear next to specific types of method objects:

- Blue Up Arrow: indicates this method overrides a method in a super class
- Blue Down Arrow: indicates this method is overridden in a subclass

### Source Pane

In the System Browser lower code source pane, you will see the following icons appear which have the following meaning:

- Orange Smudge top right: the source has changed and should be saved
- Yellow Stripe right margin: a long or complicated method that needs refactoring
- Yellow Padlock top right: when locked, indicates the current method source will be displayed stacked with other locked methods in the source pane

## Resources

If you need more detailed help, here are some additional places to look.

### ProfStef Tutorial

Try using the Pharo Smalltalk tutorial by evaluating (CMD+D) the code below:

```
	ProfStef go.
```

### Online Resources

You can find more information about Pharo by visiting:

- [https://pharo.org](https://pharo.org)
- [https://mooc.pharo.org](https://mooc.pharo.org)

In particular, you may also be interested in:

- Joining discussions and getting help at: https://pharo.org/community
- Pharo Books: [https://books.pharo.org](https://books.pharo.org)
- Reporting problems: [https://pharo.org/contribute](https://pharo.org/contribute)
