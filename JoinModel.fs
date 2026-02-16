module JoinModel

open TableModel

type JoinType =
    | Inner
    | Left
    | Right
    | Full
    override this.ToString() =
        match this with
        | Inner -> "INNER JOIN"
        | Left -> "LEFT JOIN"
        | Right -> "RIGHT JOIN"
        | Full -> "FULL JOIN"


type Join = {
    JoinType: JoinType
    LeftTable: Table
    RightTable: Table
    LeftColumn: Column
    RightColumn: Column
}
with
    override this.ToString() =
        let joinTypeStr = this.JoinType.ToString()
        let rightTableDisplay = this.RightTable.TableDisplay()
        let leftName = this.LeftColumn.ColumnRef()
        let rightName = this.RightColumn.ColumnRef()
        sprintf "%s %s ON %s = %s"
            joinTypeStr
            rightTableDisplay
            leftName
            rightName