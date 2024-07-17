; '((1 2) (2 4) (IN 1) (IN 3) (1 3) (4 5) (5 OUT))

(defun rev (L res) (cond
    ((null L) res)
    (T (rev (cdr L) (cons (car L) res)))
))

(defun reverse_list (L) (rev L nil))


; преобразовать данные 
(defun add_doors_trans (from to res) (cond
    ((null res) (cons (cons from (cons to nil)) nil))
    ((eq (caar res) from) (cons (cons (caar res) (cons to (cdar res))) (cdr res)))
    (T (cons (car res) (add_doors_trans from to (cdr res))))
))

(defun get_doors_to (L res) (cond
    ((null L) res)
    (T (get_doors_to (cdr L) (add_doors_trans (caar L) (cadar L) (add_doors_trans (cadar L) (caar L) res))))
))
;


; проверить входили мы уже в эту комнату
(defun check_not_in (door visited) (cond
    ((null visited))
    ((eq (car visited) door) nil)
    (T (check_not_in door (cdr visited)))
))

; выдать все комнаты, в которые мы можем попасть из данной
(defun get_rooms (room doors) (cond
    ((null doors) nil) 
    ((eq (caar doors) room) (cdar doors))
    (T (get_rooms room (cdr doors)))
))

; шагаем по вершинам
(defun go_to_room (to doors rooms visited res) (cond
    ((null rooms) nil)
    
    ((check_not_in (car rooms) visited) 
        (let (
                (supp_res (DFS to doors (car rooms) (cons (car rooms) visited) (cons (car rooms) res)))
             )
             (cond
                (supp_res)
                (T (go_to_room to doors (cdr rooms) (cons (car rooms) visited) res))
             )
        )
    )

    (T (go_to_room to doors (cdr rooms) visited res))
))

; поиск в глубину
(defun DFS (to doors room visited res) (cond
    ((eq room to) (reverse_list res))
    (T (go_to_room to doors (get_rooms room doors) visited res))
))

; выдать граф
(defun get_graph () 
    '((IN 1) (IN 3) (1 2) (1 5) (1 3) (5 OUT) (2 4) (3 4) (4 5) (4 6) (6 7) (6 8) (7 8) (7 9) (8 OUT))
)

; основная функция
(defun get_way (from to graph) (DFS to (get_doors_to graph nil) from (cons from nil) (cons from nil)))


(print (get_way 'IN 'OUT (get_graph)))
