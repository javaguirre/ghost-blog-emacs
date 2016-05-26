# Ghost blog package for Emacs

[![Build Status](https://travis-ci.org/javaguirre/ghost-blog-emacs.svg?branch=master)](https://travis-ci.org/javaguirre/ghost-blog-emacs)

## Requirement

The package uses `markdown-mode` for highlighting.

## Installation

```
M-x package-install ghost-blog
```

## Usage

### Configuration

You need to set up this variables for the package to work.

There is not Oauth for now, [you need to get a Bearer token](http://api.ghost.org/docs/client-authentication).

```elisp
(setq ghost-blog-url "https://javaguirre.net/ghost/api/v0.1")
(setq ghost-blog-bearer-token "Bearer my bearer"
```

### Commands

#### Show post list

Shows a clickable list of the last posts of your blog.

```elisp
(ghost-blog-get-posts)
```

#### Create new post template

It will create a new buffer with a default post template.

```elisp
(ghost-blog-new-post)
```

#### Create post

It will create a post based on the information
in the current buffer.

```elisp
(ghost-blog-save-new-post)
```

#### Update a post

After clicking a post from
the post list and doing some changes in the
buffer, you could call this command to update the
current post.

```elisp
(ghost-blog-update-post)
```

## Run unittests

We use ERT to run the unittests.

In the root of the project.

```bash
emacs -batch -l ert -l ghost-blog.el -l tests/ert.el -f ert-run-tests-batch-and-exit
```

or

```bash
make test
```
