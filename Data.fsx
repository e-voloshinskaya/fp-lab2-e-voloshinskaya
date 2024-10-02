// Часть 2. Запросы к базе данных студентов (формат базы - "One.fsx")

// Load the data
#load "One.fsx"
open One

// Task 3.1. Для каждого студента, напечатайте его среднюю оценку и сдал ли он сессию (все оценки > 2)
let averageMarks =
    Data.marks
    |> List.groupBy (fun (student, _, _) -> student) // группируем по формату: ("Петров", ["Петров","LP",4], ["Петров","MTH",4], ...); ...
    |> List.map (fun (student, values) ->
        let isFail = values |> List.exists (fun (_, _, mark) -> mark = 2) // проверяем, есть ли двойки
        let averageMark = List.averageBy (fun (_, _, mark) -> float mark) values // считаем средний балл
        student, averageMark, not isFail) // записываем в переменную-список


// Task 3.2. Для каждого предмета, напечатайте список двоечников
let studentsFailed =
    Data.marks
    |> List.filter (fun (_, _, mark) -> mark = 2) // ищем строки с оценкой 2
    |> List.groupBy (fun (_, subject, _) -> subject) // группируем их по предмету
    |> List.map (fun (subject, values) -> 
        let students = values |> List.map (fun (student, _, _) -> student) // извлекаем фамилии студентов
        subject, students)

// ищем полное название для аббревиатур предметов в Data.subjs, если существует
let findSubjectFull (subject: string) =
    match List.tryFind (fun (k, _) -> k = subject) Data.subjs with
    | Some (_, value) -> value
    | _ -> ""


// Task 3.3. Для каждой группы, найдите студента (или студентов) с максимальной суммарной оценкой
let maxTotalScore =
    Data.marks 
    |> Seq.groupBy (fun (student, _, _) -> (student, Seq.pick (fun (s, g) -> // для каждого студента ищем его группу в Data.studs
        if s = student then Some g else None) Data.studs)) // и группируем оценки по паре (студент, группа)
    |> Seq.map (fun ((student, group), marks) -> 
        (student, group, Seq.sumBy (fun (_, _, mark) -> mark) marks)) // создаем новую последовательность, суммируя оценки для каждого студента
    |> Seq.groupBy (fun (_, group, _) -> group) // группируем (студент, группа, сумм. оценка) по учебной группе
    |> Seq.map (fun (group, students_info) -> 
        let maxStudent_info = Seq.maxBy (fun (_, _, totalScore) -> totalScore) students_info
        (group, maxStudent_info)) // выбираем студента с максимальным баллом для каждой группы и создаем послед-ть (группа, студент, группа, макс. сумм. балл)
    |> Seq.map (fun (group, (student, _, _)) -> (group, student)) // создаем посл-ть (студент, макс. сумм. балл)
    |> List.ofSeq // создаем список из посл-ти
    |> List.sortBy (fun (k, _) -> k) // сортируем по возрастанию номера группы 


printfn "\tTask 3.1: Средняя оценка студента и сдал ли он сессию\n"
for (student, averageMark, isFail) in averageMarks do
    printfn "%-20s %f  %-5b" student averageMark isFail
printf "\n\n\n"

printfn "\tTask 3.2: Список двоечников по предметам\n"
for (subject, students) in studentsFailed do
    let subjectFull = findSubjectFull subject // для каждого предмета ищем полное название
    printf "%-35s |  " subjectFull
    for student in students do
        printf "%s; " student
    printfn ""
printf "\n\n\n"

printfn "\tTask 3.3: Студенты с максимальной суммарной оценкой по группам\n"
for (group, student) in maxTotalScore do
    printfn "%d: %s" group student
