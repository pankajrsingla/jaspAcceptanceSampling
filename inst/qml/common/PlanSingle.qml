//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program. If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0

Group
{
	property string suffix: ""
	IntegerField { name: "lotSize" + suffix; label: qsTr("Lot size (N)"); defaultValue: 1000; min: 1 }
	IntegerField { name: "sampleSize" + suffix; label: qsTr("Sample size (n)"); defaultValue: 100; min: 1; }
	IntegerField { name: "acceptNumber" + suffix; label: qsTr("Acceptance number (c)"); id: acceptNumberSingle; defaultValue: 4 }
	IntegerField { name: "rejectNumber" + suffix; label: qsTr("Rejection number (r)"); id: rejectNumberSingle; value: parseInt(acceptNumberSingle.value) + 1; enabled: false }
}