# HSI Halfspace Intersection: Calculate and draw convex polytopes from half-space equations

## Examples

<img src="./images/hsiexamples.svg"/>

# Basics

## Hyperplanes and Halfspaces

In the euclidian space R<sup>n</sup> a hyperplane is a flat (n-1) dimensionl space.
In R<sup>3</sup> a hyperplane is a normal 2-dimensional plane, in R<sup>2</sup> a hyperplane is
just a straight line.

Algebraically a hyperplane can be described with a linear expression of the form

a<sub>1</sub>x<sub>1</sub> + a<sub>2</sub>x<sub>2</sub> + ... + a<sub>n</sub>x<sub>n</sub> + d

with fixed coefficients a<sub>1</sub>,a<sub>2</sub>, ..., a<sub>n</sub>, and d.  A hyperplane
partitions the euclidian space into 3 distinct and convex areas:

* H<sup>-</sup> or H-: All points (x<sub>1</sub>,x<sub>2</sub>, ..., x<sub>n</sub>) where the linear expression evaluates to a negative value. This is called the *negative open half-space*.
* H<sup>0</sup> or H0: All points (x<sub>1</sub>,x<sub>2</sub>, ..., x<sub>n</sub>) where the linear expression evaluates to 0.
* H<sup>+</sup> or H+: All points (x<sub>1</sub>,x<sub>2</sub>, ..., x<sub>n</sub>) where the linear expression evaluates to a positive value. This is called the *positive open half-space*.

The union of the point sets

* H<sup>-</sup> and H<sup>0</sup> is called the *negative closed half-space*.
* H<sup>+</sup> and H<sup>0</sup> is called the *positive closed half-space*. In this project
a *half-space* is always a positive closed half-space.

<center><img src="./images/Halfspace.svg"/></center>

## Convex Polytopes

A convex polytope is the intersection of finitely many half-spaces in R<sup>n</sup>.
In the sequel a polytope means always a convex polytope.

From a set of half-spaces the software in this repository calculates all faces of the polytope.
In R<sup>3</sup> this software also creates simple drawings.

<center><img src="./images/Intersect.svg"/></center>

## Data Structure

All vertices, edges, sides and  hyperplanes are called *face*s. Set inclusion of the point sets of the faces defines a
half-order relation on the faces. A half-orders can be represented with a *Hasse diagram* or a *face lattice diagram*. This is
a special form of a *directed acyclic graph*. In Haskell we can use *Map*s to implement directed acyclic graphs.

The next 2 images show all faces of a normal triangle as a geometric object and as a face diagram:

<center><img src="./images/Hasse.svg"/></center>

## Algorithms on Hasse Diagrams

Similar to trees, algorithms can traverse the nodes (aka faces) of a Hasse diagram in preorder or
in postorder method. Different to trees, the traversal method can visit a node multiple times (*Multiple visits*) or only once (*Single visit*).

To specify an algorithm on a Hasse diagram we have to specify the following:

* Traversal method (preorder or postorder)
* Visting frequency (Single or Multiple)
* The processing for each node / face.

The numbers in the nodes in the following images, represent the visiting sequence.

### Postorder Visiting Sequence (or bottom up)

<center><img src="./images/Postorder.svg"/></center>

### Preorder Visiting Sequence (or top down)

<center><img src="./images/Preorder.svg"/></center>

# HSI Algorithm

The algorithm to calculate a polytope from a set of halfspaces starts with a
huge cube and applies the HSI algorithm for each halfspace in the set.

To visualize the algorihm, we use a simple triangle. The red line is the half-space, the little
arrow points towards H<sup>+</sup>.

<img src="./images/Algo01.svg"/>

The following sctions contain a short description of the algorithm.

## 1. Calculate the Relative Position for each Face

The relative position to a halfspace for a vertex is either:

* **"0"** : if the vertex is part of H<sup>0</sup>
* **"+"** : if the vertex is part of H<sup>+</sup>
* **"-"** : if the vertex is part of H<sup>-</sup>

The relative position of a non-vertex face is the *OR*-combination of all vertices of this face.
eg For an edge with vertices on both sides of H<sup>0</sup> the relative position is **"-+"**.

This is implemented by a postorder walk with single visits to the faces/nodes. For each face, the
following diagram shows the calculated relative position:

<img src="./images/Algo02.svg"/>

## 2. Intersection with H<sup>-</sup>

The intersection with H<sup>-</sup> is easy:

Algorithm: Postorder, Single visit.

Node processing: Remove every node with relative position "-" or "-0". Put keys

<img src="./images/Algo03.svg"/>

## 3. Intersection with H<sup>0</sup>

<img src="./images/Algo04.svg"/>

<img src="./images/Algo05.svg"/>

# Higher Dimensions

# Input Program

# Terminology

Terminology is from [2]. We mainly use:

* 0-dim-faces are called `vertices`,
* 1-dim-faces are called `edges`,
* (dim(P) − 1)-faces are called `facets`.
* The dim(P) face is called `polytope`.


### References

[1] [Nef-W. (1978). Beiträge zur Theorie der Polyeder. Bern: Herbert Lang](https://books.google.ch/books/about/Beitr%C3%A4ge_zur_Theorie_der_Polyeder.html?id=3Lm0AAAAIAAJ&redir_esc=y)

[2] [Welz-E, Gärtner-B. (2020). Theory of Combinatorial Algorithms, Chapter 9 ](https://ti.inf.ethz.ch/ew/courses/Geo20/lecture/gca20-9.pdf)

[3] [Article on Convex Polytopes on Wikipedia](https://en.wikipedia.org/wiki/Convex_polytope)