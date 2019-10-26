;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |exercise 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "./remove_duplicates.rkt")


;;Linus Brian Okoth
;;lbo0341

; This defines the basic album datatype.
(define-struct album (title artist genre))
; define-struct automatically creates the following functions for you:
;
; `make-<struct-name>` (in this case `make-album`)
;   a function to create an instance of the struct
;   this function takes arguments for each of the fields listed, so for example
;   (make-struct 'Sway' 'Tove Styrke' 'Pop') will create an album struct
;    with title 'Sway', artist 'Tove Styrke' & genre 'Pop
;
; `<struct-name>-<field-name>` (for each field)
;    functions for accessing values of each field in the struct
;    for album this would mean we'd have the following functions:
;    `album-title`, `album-artist`, `album-genre`
;    the following examples creates an album and then accesses its fields
;    ```
;    (define sway (make-album 'Sway' 'Tove Styrke' 'Pop')
;    (album-title sway) ; returns 'Sway'
;    (album-artist sway) ; returns 'Tove Styrke'
;    (album-genre sway) ; returns 'Pop'
;    ```
;
; `<struct-name>?` (in this case `album?`)
;   a predicate (function which returns a boolean) that checks a value and
;   returns true if it's an instance of the struct, false otherwise
;   using the `sway` album defined in the previous example
;   ```
;   (album? sway) ; returns true
;   (album? 1) ; returns false
;   (album? 'hi') ; returns false
;   ```

;;; Enter a list of albums below
;;; They need not be the actual albums you own.
;;; But you should include enough variety to adequately
;;; test your code.
;;;
;;; Here's what we mean.  One of the questions involves
;;; writing a procedure that finds all the albums of a
;;; given genre.  If all the albums in the library are
;;; in the rock genre, then there's only one genre and
;;; when you ask for all the rock albums and it gives
;;; back all the albums, you don't know whether that's
;;; because the code really works, or because it's
;;; not even paying attention to the genre.  So you want
;;; to make sure there are multiple artists and genres,
;;; some artists with only one album or genre, others
;;; with multiple artists or genres, etc.

(define testing-library-1
  ;; Fill in the info below
  (list (make-album "Blonde" "Frank" "R&B")
        (make-album "Malibu" "Anderson" "Hip-Hop")
        (make-album "Emotion" "Carly" "Pop")
        (make-album "Rude" "Carly" "Pip")
        (make-album "Boom" "Liz" "Pop")
        (make-album "Jump" "Carly" "Pop")
        (make-album "Go" "Liz" "R&B")
        ))

;;; Add the procedures you write (e.g. all-genres, versatile-artists)
;;; below.  Be sure to test your procedures to make sure they work.
;;; We are not providing test cases this time, so it's up to you
;;; to make sure your code works.  We will use our own test cases
;;; when grading and assign you a grade based on the number of
;;; test cases that passed.


;;Signature: all-titles : (listof album) -> (listof string)
;;Purpose: To find all the titles of albums in the library

(define all-titles
  (lambda (lib)
    (map album-title lib)
    ))

(check-expect (all-titles testing-library-1) (list "Blonde" "Malibu" "Emotion" "Rude" "Boom" "Jump" "Go")) 

;;Signaature: all-artists: (listof album) -> (listof string)
;;Purpose: to get a non duplicated list of names of artist with albums in the library 

(define all-artists
  (lambda (lib)
    (remove-duplicates (map album-artist lib))))


(check-expect (all-artists testing-library-1) (list "Frank" "Anderson" "Carly" "Liz"))                        


;;Signature: all-genres: (listof album) -> (listof string)
;;Purpose: to find non duplicated list of all the genres of albums in the library

(define all-genres
  (lambda (lib)
    (remove-duplicates (map album-genre lib))))

(check-expect (all-genres testing-library-1) (list "R&B" "Hip-Hop" "Pop" "Pip"))


;; artist-albums : string, (listof album) -> (listof album)
;; Get all albums in the library by the given artist

(define artist-albums
  (lambda (artist lib)
    (filter (lambda (name)
              (string=? artist (album-artist name)))
            lib)))
              
            
(check-expect (artist-albums "Carly" testing-library-1) (list
                                                         (make-album "Emotion" "Carly" "Pop")
                                                         (make-album "Rude" "Carly" "Pip")
                                                         (make-album "Jump" "Carly" "Pop")))             



;; artist-genres: string, (listof album) -> (listof string)
;; Fetch all the genres of  given artist in the library

(define artist-genres
  (lambda (artist lib)
    (all-genres (artist-albums artist lib))))

(check-expect (artist-genres "Carly" testing-library-1) (list "Pop" "Pip"))



;; artist-is-versatile?: string, (listof album) -> boolean
;; Checks whether an artist has an album in more thab one genre and returns true

(define artist-is-versatile?
  (lambda (artist lib)
    (if (> (length (artist-genres artist lib)) 1)
        #t
        #f)))

(check-expect (artist-is-versatile? "Carly" testing-library-1) #t)
     
    



;; versatile-artists: (listof album) -> (listof string)
;; Fetchs the names of all artist who have an album in more than one genre

(define versatile-artists
  (lambda (lib)
    (filter (lambda (n)
              (artist-is-versatile? n lib))
            (all-artists lib))))

(check-expect (versatile-artists testing-library-1) (list "Carly" "Liz"))



;; artist-album-counts: (listof album) -> (listof (list string number))
;; displays all artists with albums in the library and the number of albums they have

(define artist-album-counts
  (lambda (lib)
    (map (lambda (artist)
           (list artist (length (artist-albums artist lib))))
         (all-artists lib))))


         
(check-expect (artist-album-counts testing-library-1)(list
                                                      (list "Frank" 1)
                                                      (list "Anderson" 1)
                                                      (list "Carly" 3)
                                                      (list "Liz" 2)))


;; genre-album-counts: (listof album) -> (listof (list string number))
;; Displays all genres in the library and how many albums in each


(define genre-album-counts
  (lambda (lib)
    (map (lambda (genre)
           (list genre (length ((lambda (genre lib)
                                  (filter (lambda (name)
                                            (string=? genre (album-genre name)))
                                          lib)) genre lib))))
         (all-genres lib))))

(check-expect (genre-album-counts testing-library-1)(list
                                                      (list "R&B" 2)
                                                      (list "Hip-Hop" 1)
                                                      (list "Pop" 3)
                                                      (list "Pip" 1)))
