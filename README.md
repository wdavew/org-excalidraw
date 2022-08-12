# org-excalidraw

## Introduction

`org-excalidraw` is a package for managing [excalidraw](https://www.excalidraw.com) drawings in org-mode.

It supports creating and linking to a new excalidraw file from an org-mode file


## Why?

### Excalidraw
I use excalidraw a lot. It's an excellent tool, but I have trouble organizing my drawings.

I usually end up with one massive canvas containing a bunch of unrelated diagrams haphazardly arranged.

Then I copy and paste portions out into documents as needed.

### Org Mode

`org-mode` is my go to for note taking and organization.
One of the few components `org-mode` lacks is a solution to easily draw when plain text just doesn't cut it.

### Org-Excalidraw

`org-excalidraw` provides a barebones solution: call out to excalidraw from a .org file to link to drawings. You can also render them inline in org-mode.
This package also implements a custom link type for excalidraw files. Following the excalidraw link will open your drawing in excalidraw.
Saving changes in excalidraw will be reflected in org mode within a second or two.

For me, this synchronization makes both tools more useful: I can easily draw in org-mode, and I can apply the organizational capabilities of org-mode to excalidraw diagrams.

## Requirements

Excalidraw is a web application. `org-excalidraw` takes advantage of the File Handling API to register Excalidraw as a file handler for `.excalidraw` files.

It also requires a few external dependencies outside of Emacs and `org-mode` for converting between excalidraw json and svg.

`org-excalidraw` has only been tested against Chromium based browsers.


## Installation

Using `use-pacakge`:

```elisp
(use-package org-excalidraw
  :straight (:type git :host github :repo "wdavew/org-excalidraw")
  :config
  (org-excalidraw-directory "~/path_to_store_excalidraw_files")
)
```


`org-excalidraw` requires external dependencies outside of Emacs and `org-mode`.

1. We need to programmatically export excalidraw files to `.svg` for display.
org-excalidraw depends on [excalidraw_export](https://github.com/Timmmm/excalidraw_export) to do this. It must be available on your system's PATH.
NOTE: you may need to install some dependencies like `canvas` for this package to work correctly.

2. To correctly display the produced SVGs, your system needs some fonts installed.
[The excalidraw_export repo provides these as well](https://github.com/Timmmm/excalidraw_export/tree/master/src).

3. Install [excalidraw](https://www.excalidraw.com) as a PWA using Chrome. After doing this, you should be able to launch excalidraw from your system as if it was an application.

4. Enable the `File Handling API` flag in Chrome. Visit `chrome://flags` to do this.

5. Install and configure `org-excalidraw`.

## Usage

Call `M-x org-excalidraw-create-drawing` in an `org-mode` document.

This does 2 things:

1. Creates an empty excalidraw file. You can adjust the settings `org-excalidraw` applies to new drawings by customizing `org-excalidraw-base`.

2. Inserts a link to this file with a custom `excalidraw:` type. The `excalidraw:` link type both displays the image inline and will open the drawing in excalidraw for editing when followed.


As long as `excalidraw_export` is available and configured correctly, all changes saved in the excalidraw application will update the corresponding svg files.
