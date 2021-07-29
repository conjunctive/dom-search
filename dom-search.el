;;; dom-search.el --- additional utilities for searching the DOM -*- lexical-binding: t -*-

;; Copyright (C) 2021  Conjunctive

;; Author: Conjunctive <conjunctive@protonmail.com>
;; Keywords: dom html web
;; Version: 0.0.2
;; URL: https://github.com/conjunctive/dom-search
;; Package-Requires: ((emacs "27") s)

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Collect multiple nodes through a shallow search.
;; For tags, `eq' is being utilized to determine equality.
;; - `dom-collect-children-by-tag'
;;
;; Locate a single node through a shallow search.
;; As soon as the node has been found, it should be returned immediately.
;; For tags, `eq' is being utilized to determine equality.
;; - `dom-find-child-by-tag'
;; - `dom-find-child-by-attr'
;; - `dom-find-child-by-class'
;;
;; Locate a single node through a deep search.
;; As soon as the node has been found, it should be returned immediately.
;; For tags, `eq' is being utilized to determine equality.
;; - `dom-find-descendant-by-tag'
;;
;; Locate a single node through a deep search.
;; As soon as the node has been found, it should be returned immediately.
;; When trimmed, a node attribute being matched upon should be identical to the MATCH argument.
;; This means `string=' is being utilized to determine equality.
;; - `dom-find-descendant-by-attr'
;; - `dom-find-descendant-by-class'
;; - `dom-find-descendant-by-id'
;;
;; Locate a single node through a deep search.
;; As soon as the node has been found, it should be returned immediately.
;; A node attribute under examination should successfully match against the MATCH argument.
;; This means `string-match' is being utilized to determine equality.
;; - `dom-find-descendant-by-attr-match'
;; - `dom-find-descendant-by-class-match'
;; - `dom-find-descendant-by-id-match'
;;
;; Locate a single node through a series of nested shallow queries.
;; Each query states that a given attribute should be equal to a given value.
;; This is useful when you are looking for a specific node at a given depth.
;; - `dom-descend'

;;; Code:

(require 'cl-macs)
(require 'dom)
(require 'simple)
(require 'subr-x)
(require 's)

(defun dom-collect-children-by-tag (match dom)
  "Perform a shallow search of the DOM tree.
Return all child nodes with the provided tag of MATCH."
  (when (and dom (listp dom))
    (cl-loop for child in (dom-children dom)
             when (and (listp child)
                       (eq match (dom-tag child)))
             collect child)))

(defun dom-find-child-by-tag (match dom)
  "Perform a shallow search of the DOM tree.
Return the first child node with the provided tag of MATCH.
Variant of `dom-child-by-tag' that short-circuits on an invalid child node."
  (when (and dom (listp dom))
    (cl-loop for child in (dom-children dom)
             when (and (listp child)
                       (eq match (dom-tag child)))
             return child)))

(defun dom-find-child-by-attr (attr match dom)
  "Perform a shallow search of the DOM tree.
Return the first child node with the provided ATTR set to MATCH.
Equality is determined using `string='.
The input will be trimmed prior to the equality check."
  (when (and dom (listp dom))
    (cl-loop for child in (dom-children dom)
             when (and (not (stringp child))
                       (when-let ((attr-val (dom-attr child attr)))
                         (string= match (s-trim attr-val))))
             return child)))

(defsubst dom-find-child-by-class (match dom)
  "Perform a shallow search of the DOM tree.
Return the first child node with the class set to MATCH.
Equality is determined using `string='.
The input will be trimmed prior to the equality check."
  (dom-find-child-by-attr 'class match dom))

(defun dom-find-descendant-by-tag (tag dom)
  "Perform a deep search of the DOM tree.
Return the first descendant node of the provided TAG."
  (if (eq tag (dom-tag dom))
      dom
    (cl-loop for child in (dom-children dom)
             for matches = (and (not (stringp child))
                                (dom-find-descendant-by-tag tag child))
             when matches
             return matches)))

(defun dom-find-descendant-by-attr (attr match dom)
  "Perform a deep search of the DOM tree.
Return the first descendant node with the provided ATTR set to MATCH.
Equality is determined using `string='.
The input will be trimmed prior to the equality check."
  (let ((node-attr (dom-attr dom attr)))
    (if (and node-attr (string= match (s-trim node-attr)))
        dom
      (cl-loop for child in (dom-children dom)
               for matches = (and (not (stringp child))
                                  (dom-find-descendant-by-attr attr match child))
               when matches
               return matches))))

(defun dom-find-descendant-by-attr-match (attr match dom)
  "Perform a deep search of the DOM tree.
Return the first descendant node where the provided ATTR matches the regex MATCH."
  (let ((node-attr (dom-attr dom attr)))
    (if (and node-attr (string-match match node-attr))
        dom
      (cl-loop for child in (dom-children dom)
               for matches = (and (not (stringp child))
                                  (dom-find-descendant-by-attr-match attr match child))
               when matches
               return matches))))

(defsubst dom-find-descendant-by-class (match dom)
  "Perform a deep search of the DOM tree.
Return the first descendant node with the class set to MATCH.
Equality is determined using `string='.
The input will be trimmed prior to the equality check."
  (dom-find-descendant-by-attr 'class match dom))

(defsubst dom-find-descendant-by-class-match (match dom)
  "Perform a deep search of the DOM tree.
Return the first descendant node with the class matching the regex MATCH."
  (dom-find-descendant-by-attr-match 'class match dom))

(defsubst dom-find-descendant-by-id (match dom)
  "Perform a deep search of the DOM tree.
Return the first descendant node with the ID set to MATCH.
Equality is determined using `string='.
The input will be trimmed prior to the equality check."
  (dom-find-descendant-by-attr 'id match dom))

(defsubst dom-find-descendant-by-id-match (match dom)
  "Perform a deep search of the DOM tree.
Return the first descendant node with the ID matching the regex MATCH."
  (dom-find-descendant-by-attr-match 'id match dom))

(defmacro dom-descend (dom matches)
  "Descend into nested DOM nodes shallowly.
Each entry in MATCHES represents a query to perform
on the child nodes per level of nesting.

This is achieved using `dom-find-child-by-attr',
which performs a shallow search of the child nodes.

An entry in MATCHES associates an attribute with a match value.

Examples:

To access the <a> tag in the following structure:
;; <??? class=\"link\">
;;   <div>
;;     <a></a>
;;   </div>
;; </???>
\(dom-descend dom ((class . \"link\")
                  (tag . div)
                  (tag . a)))

To access the node with the class name of \"c\"
in either of the following structures:
;; <??? class=\"a\">
;;   <h1>
;;     <??? class=\"c\">
;;     </???>
;;   </h1>
;; </???>
OR
;; <??? class=\"a\">
;;   <h2>
;;     <??? class=\"c\">
;;     </???>
;;   </h2>
;; </???>
\(dom-descend dom ((class . \"a\")
                  (tag . (or h1 h2))
                  (class . \"c\")))

In the above example, `or' was used to decide upon which result to keep.
You may use any defined multi-arity function (or macro) to decide upon a result.
Note that `funcall' is NOT utilized here, so anonymous functions are unsupported.

\(defun pick-something (&rest options)
  (nth (random (length options))
       options))
\(dom-descend dom ((tag . article)
                  (class . (pick-something \"header\"
                                           \"content\"
                                           \"footer\"))))"
  (cl-loop with expr = dom
           for (attribute . match) in matches
           if (or (stringp match) (symbolp match))
           do (setq expr (if (string= 'tag attribute)
                             `(dom-find-child-by-tag ',match ,expr)
                           `(dom-find-child-by-attr ',attribute ,match ,expr)))
           ;; NOTE: Assume MATCH is a list, representing an expression that picks a branch (typically 'or).
           ;; We need to introduce a binding to avoid evaluating the accumulated expression N times.
           ;;
           ;; To understand what's happening here, let's expand on some examples:
           ;;
           ;; (macroexpand-1 '(dom-descend dom ((tag . div) (tag . (or h1 h2)))))
           ;; => (let ((g? (dom-find-child-by-tag 'div dom)))
           ;;      (or (dom-find-child-by-tag 'h1 g?)
           ;;          (dom-find-child-by-tag 'h2 g?)))
           ;;
           ;; (macroexpand-1 '(dom-descend dom ((tag . div) (tag . (choose-something h1 h2)))))
           ;; => (let ((g? (dom-find-child-by-tag 'div dom)))
           ;;      (choose-something (dom-find-child-by-tag 'h1 g?)
           ;;                        (dom-find-child-by-tag 'h2 g?)))
           ;;
           else do (cl-loop with expr-var = (gensym)
                            for condition-match in (cdr match)
                            for condition-expr = (if (string= 'tag attribute)
                                                     `(dom-find-child-by-tag ',condition-match ,expr-var)
                                                   `(dom-find-child-by-attr ',attribute ,condition-match ,expr-var))
                            collect condition-expr into condition-exprs
                            finally do (setq expr `(let ((,expr-var ,expr))
                                                     (,(car match) ,@condition-exprs))))
           finally return expr))

(provide 'dom-search)

;;; dom-search.el ends here
